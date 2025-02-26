package org.tp.process_time_state

import scala.annotation.targetName

import cats.data.NonEmptyChain
import cats.implicits.*
import cats.syntax.validated.*
import cats.ApplicativeError

final type Behavior[-C, S, +EE <: Error] = PartialFunction[(C, S), InvariantError[EE, S]]
final type BehaviorsK[F[_], -C, S]       = (C, S) => F[S]

final type Output[-C, S, +E]         = PartialFunction[(C, S), E]
final type OutputsK[F[_], -C, -S, E] = (C, S) => F[E]

extension [F[_], C, S, EE <: Error](behaviors: BehaviorsK[F, C, S])
  private def onlyWhenLifecycleIsActive(using
      F: ApplicativeError[F, NonEmptyChain[EE]],
      isFinal: HasEnded[S],
  ): BehaviorsK[F, C, S] =
    (c: C, s: S) =>
      if !isFinal(s) then behaviors(c, s)
      else F.raiseError(NonEmptyChain.one(LifecycleHasEnded(c, s).asInstanceOf[EE]))

extension [C, S, EE <: Error](behavior: Behavior[C, S, EE])

  def guard(
      invariants: List[Invariant[C, S, EE]] = Nil,
  ): Behavior[C, S, EE] = { case (c: C, s: S) =>
    if !behavior.isDefinedAt((c, s)) then
      NonEmptyChain.one(BehaviorIsNotDefined(c, s).asInstanceOf[EE]).invalid
    else
      invariants
        .map(f => f.check((c, s)))
        .sequence
        .andThen(_ => behavior(c, s))
  }

  @targetName("withGuards")
  /** Guards a Transition with a List of Invariants */
  def <<<(invariants: List[Invariant[C, S, EE]]): Behavior[C, S, EE] =
    guard(invariants)

  @targetName("withGuard")
  /** Guards a Transition with single Invariant */
  def <<(invariant: Invariant[C, S, EE]): Behavior[C, S, EE] =
    guard(List(invariant))

  @targetName("liftBehavior")
  def liftF[F[_]](using
      F: ApplicativeError[F, NonEmptyChain[EE]],
  ): BehaviorsK[F, C, S] =
    (c: C, s: S) =>
      if !behavior.isDefinedAt((c, s)) then
        F.raiseError(NonEmptyChain.one(BehaviorIsNotDefined(c, s).asInstanceOf[EE]))
      else behavior((c, s)).fold(F.raiseError, F.pure)

  def liftLifecycleF[F[_]](using
      F: ApplicativeError[F, NonEmptyChain[EE]],
      isFinal: HasEnded[S],
  ): BehaviorsK[F, C, S] = behavior.liftF.onlyWhenLifecycleIsActive

extension [C, S, E](output: Output[C, S, E])
  @targetName("liftOut")
  def liftF[F[_], EE <: Error](using
      F: ApplicativeError[F, NonEmptyChain[EE]],
  ): OutputsK[F, C, S, E] =
    (c: C, s: S) =>
      if !output.isDefinedAt((c, s)) then
        F.raiseError(NonEmptyChain.one(OutputIsNotDefined(c, s).asInstanceOf[EE]))
      else F.pure(output((c, s)))

/** No Behavior has been defined for a (cmd, state) pair. */
private final case class BehaviorIsNotDefined[C, S](c: C, s: S) extends Error {
  override def msg: String = s"Behavior for input label ($c, $s) is not defined."
}

/** No Output has been defined for a (cmd, state) pair. */
private final case class OutputIsNotDefined[C, S](c: C, s: S) extends Error {
  override def msg: String = s"Output for input label ($c, $s) is not defined."
}

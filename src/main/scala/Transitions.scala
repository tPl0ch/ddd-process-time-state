package org.tp.process_time_state

import cats.{ ApplicativeError, ApplicativeThrow }
import cats.data.{ Kleisli, NonEmptyChain }
import cats.implicits.*
import cats.syntax.validated.*
import scala.annotation.targetName

import Invariants.*
import Lifecycle.{ HasEnded, LifecycleHasEnded }

type TransitionsK[F[_], -C, -S, E] = Kleisli[F, (C, S), E]

type Behavior[-C, S, +EE <: Error] = PartialFunction[(C, S), InvariantError[EE, S]]
type BehaviorsK[F[_], -C, S]       = TransitionsK[F, C, S, S]

type Output[-C, S, +E]         = PartialFunction[(C, S), E]
type OutputsK[F[_], -C, -S, E] = TransitionsK[F, C, S, E]

object Transitions {

  /** No Behavior has been defined for a (cmd, state) pair. */
  final case class BehaviorIsNotDefined[C, S](c: C, s: S) extends Error {
    override def msg: String = s"Transition for input label ($c, $s) is not defined."
  }

  /** No Output has been defined for a (cmd, state) pair. */
  final case class OutputIsNotDefined[C, S](c: C, s: S) extends Error {
    override def msg: String = s"Event for input label ($c, $s) is not defined."
  }

  extension [F[_], C, S](behaviors: BehaviorsK[F, C, S])
    def onlyWhenLifecycleIsActive(using
        F: ApplicativeThrow[F],
        isFinal: HasEnded[S],
    ): BehaviorsK[F, C, S] =
      Kleisli { (c: C, s: S) =>
        if !isFinal(s) then behaviors((c, s))
        else F.raiseError(LifecycleHasEnded(c, s))
      }

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
      Kleisli { (c: C, s: S) =>
        if !behavior.isDefinedAt((c, s)) then
          F.raiseError(NonEmptyChain.one(BehaviorIsNotDefined(c, s).asInstanceOf[EE]))
        else behavior((c, s)).fold(F.raiseError, F.pure)
      }

  extension [C, S, E](output: Output[C, S, E])
    @targetName("liftOut")
    def liftF[F[_], EE <: Error](using
        F: ApplicativeError[F, NonEmptyChain[EE]],
    ): OutputsK[F, C, S, E] =
      Kleisli { (c: C, s: S) =>
        if !output.isDefinedAt((c, s)) then
          F.raiseError(NonEmptyChain.one(OutputIsNotDefined(c, s).asInstanceOf[EE]))
        else F.pure(output((c, s)))
      }
}

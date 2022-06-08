package org.tp.process_time_state

import Lifecycle.IsEnd

import cats.data.*
import cats.data.Validated.{ Invalid, Valid }
import cats.implicits.*
import cats.{ ApplicativeError, FlatMap, Monad }

import scala.annotation.targetName

/** The Transitions[F] trait provides the Aggregate instance with defining state transitions and
  * invariants.
  */
trait Transitions[F[_]] { self: Aggregate[F] =>

  /** The (Command, State) input label */
  final type LabelIn = (C, S)

  /** We are accumulating domain errors in a NonEmptyList */
  final type NEC = NonEmptyChain[EE]

  /** We use the accumulating Validated structure */
  final type InvariantError = Validated[NEC, Unit]

  /** An Invariant is a partial function that either gives a valid Unit value, or an accumulating
    * error structure.
    */
  final type Invariant = PartialFunction[LabelIn, InvariantError]

  /** A Transition is a partial function (not all combinations of commands and states need to have a
    * transition defined) that maps an input label to a state.
    */
  final type Transition = PartialFunction[LabelIn, F[S]]

  /** A TransitionF is just an alias to Kleisli[F, LabelIn, S]. Kleisli structures are
    * representations of functions C => F[S] - exactly what we need to model a function that maps a
    * Command to a State within the context of F.
    */
  final type TransitionF = Kleisli[F, LabelIn, S]

  /** This abstract method needs to provide all the state transitions supported by the Aggregate.
    * You can use the `mkTransitionF` helper method to easily lift the composed partial functions
    * into the Kleisli data structure.
    *
    * @see
    *   mkTransitionF
    */
  def transitions: TransitionF

  /** This error is indicated when there is no TransitionF for a LabelIn.
    */
  private final case class TransitionNotDefined(l: LabelIn) extends DomainError {
    override def msg: String = s"Transition is not defined for command ${l._1} and state ${l._2}"
  }

  /** This error is indicated when there is no Invariant for a LabelIn on a specific TransitionF */
  private final case class InvariantCannotBeChecked(l: LabelIn) extends DomainError {
    override def msg: String =
      s"An Invariant could not be checked for command ${l._1} and state ${l._2}"
  }

  /** This error indicates when the Aggregate is in a final state and can't process any more
    * commands
    */
  private final case class LifecycleHasEnded(l: LabelIn) extends DomainError {
    override def msg: String = s"Command ${l._1} cannot be processed because state ${l._2} is final"
  }

  extension (invariant: Invariant)
    @targetName("maybeInvariant")
    final def maybe: Invariant =
      (l: LabelIn) =>
        if !invariant.isDefinedAt(l) then ().validNec
        else invariant(l)

  extension (transition: Transition)

    final def guard(
        invariants: List[Invariant] = Nil,
    )(using F: ApplicativeError[F, NEC]): Transition = (command: C, state: S) => {
      invariants
        .map(f => f.maybe((command, state)))
        .sequence match
        case Valid(_)                        => maybe((command, state))
        case Invalid(nec: NonEmptyChain[EE]) => F.raiseError(nec)
    }

    @targetName("withGuards")
    /** Guards a Transition with a List of Invariants */
    final def <<<(invariants: List[Invariant])(using F: ApplicativeError[F, NEC]): Transition =
      guard(invariants)

    @targetName("withGuard")
    /** Guards a Transition with single Invariant */
    final def <<(invariant: Invariant)(using F: ApplicativeError[F, NEC]): Transition =
      guard(List(invariant))

    @targetName("maybeTransition")
    final def maybe(using F: ApplicativeError[F, NEC]): Transition =
      Transitions.maybe(transition, l => TransitionNotDefined(l).asInstanceOf[EE])

    @targetName("liftTransition")
    final def liftK(using
        F: ApplicativeError[F, NEC],
        isFinal: IsEnd[S],
    ): TransitionF = Kleisli { (c: C, s: S) =>
      if isFinal(s) then
        NonEmptyChain.one(LifecycleHasEnded((c, s)).asInstanceOf[EE]).raiseError[F, S]
      else maybe((c, s))
    }
}

object Transitions {
  def maybe[F[_], IN, OUT, EE <: DomainError](
      partialFunction: PartialFunction[IN, F[OUT]],
      e: IN => EE,
  )(using F: ApplicativeError[F, NonEmptyChain[EE]]): PartialFunction[IN, F[OUT]] =
    (in: IN) =>
      if !partialFunction.isDefinedAt(in) then F.raiseError(NonEmptyChain.one(e(in)))
      else partialFunction(in)
}

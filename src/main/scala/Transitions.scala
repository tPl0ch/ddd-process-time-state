package org.tp.process_time_state

import Lifecycle.IsOmega

import cats.ApplicativeError
import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ Kleisli, NonEmptyList, ValidatedNel }
import cats.implicits.*

import scala.annotation.targetName

/** The Transitions[F] trait provides the Aggregate instance with defining state transitions and
  * invariants.
  */
trait Transitions[F[_]] { self: Aggregate[F] =>

  /** An Invariant is a partial function that either gives a valid Unit value, or an accumulating
    * error structure
    */
  final type Invariant = PartialFunction[LabelIn, ValidatedNel[EE, Unit]]

  /** A Transition is a partial function (not all combinations of commands and states need to have a
    * transition defined) that maps an input label to a state.
    */
  final type Transition = PartialFunction[LabelIn, F[S]]

  /** A TransitionF is just an alias to Kleisli[F, LabelIn, S]. Kleisli structures are
    * representations of functions C => F[S] - exactly what we need to model a function that maps a
    * Command to a State within the context of F.
    */
  final type TransitionF = Kleisli[F, LabelIn, S]

  /** This type is representing a final state within the Aggregate's context */
  final type IsFinal = IsOmega[S]

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

  /** Lifts a Transition into TransitionF */
  final protected def mkTransitionF(
      transitionsToBeLifted: Transition,
  )(using
      applicativeError: ApplicativeError[F, NEL],
      isFinal: IsFinal,
  ): TransitionF = Kleisli { (c: C, s: S) =>
    if isFinal(s) then NonEmptyList.of(LifecycleHasEnded((c, s)).asInstanceOf[EE]).raiseError[F, S]
    else maybeDefinedTransition(transitionsToBeLifted)((c, s))
  }

  final private def maybeDefinedTransition(
      transition: Transition,
  )(using applicativeError: ApplicativeError[F, NEL]): Transition =
    (l: LabelIn) =>
      if !transition.isDefinedAt(l) then
        applicativeError.raiseError(NonEmptyList.of(TransitionNotDefined(l).asInstanceOf[EE]))
      else transition(l)

  final private def maybeDefinedInvariant(invariant: Invariant): Invariant =
    (l: LabelIn) =>
      if !invariant.isDefinedAt(l) then InvariantCannotBeChecked(l).asInstanceOf[EE].invalidNel
      else invariant(l)

  final private def mkTransition(
      transitionToBeGuarded: Transition,
      invariants: List[Invariant] = Nil,
  )(using
      applicativeError: ApplicativeError[F, NEL],
  ): Transition = (command: C, state: S) => {
    invariants
      .map(f => maybeDefinedInvariant(f)((command, state)))
      .sequence match
      case Valid(_)          => maybeDefinedTransition(transitionToBeGuarded)((command, state))
      case Invalid(nel: NEL) => applicativeError.raiseError(nel)
  }

  extension (transition: Transition)
    @targetName("withGuards")
    /** Guards a Transition with a List of Invariants */
    final def <<<(
        invariants: List[Invariant],
    )(using
        applicativeError: ApplicativeError[F, NEL],
    ): Transition = mkTransition(
      transition,
      invariants,
    )

    @targetName("withGuard")
    /** Guards a Transition with single Invariant */
    final def <<(
        invariant: Invariant,
    )(using
        applicativeError: ApplicativeError[F, NEL],
    ): Transition = mkTransition(
      transition,
      List(invariant),
    )
}

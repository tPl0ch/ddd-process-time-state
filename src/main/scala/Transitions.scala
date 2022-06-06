package org.tp.process_time_state

import Lifecycle.IsEnd

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
        if !invariant.isDefinedAt(l) then ().validNel
        else invariant(l)

  extension (transition: Transition)

    final def guard(invariants: List[Invariant] = Nil)(using
        applicativeError: ApplicativeError[F, NEL],
    ): Transition = (command: C, state: S) => {
      invariants
        .map(f => f.maybe((command, state)))
        .sequence match
        case Valid(_)          => maybe((command, state))
        case Invalid(nel: NEL) => applicativeError.raiseError(nel)
    }

    @targetName("withGuards")
    /** Guards a Transition with a List of Invariants */
    final def <<<(
        invariants: List[Invariant],
    )(using
        applicativeError: ApplicativeError[F, NEL],
    ): Transition = guard(invariants)

    @targetName("withGuard")
    /** Guards a Transition with single Invariant */
    final def <<(
        invariant: Invariant,
    )(using
        applicativeError: ApplicativeError[F, NEL],
    ): Transition = guard(List(invariant))

    @targetName("maybeTransition")
    final def maybe(using applicativeError: ApplicativeError[F, NEL]): Transition =
      Aggregate.maybe(transition, l => TransitionNotDefined(l).asInstanceOf[EE])

    @targetName("liftTransition")
    final def liftK(using
        applicativeError: ApplicativeError[F, NEL],
        isFinal: IsEnd[S],
    ): TransitionF = Kleisli { (c: C, s: S) =>
      if isFinal(s) then
        NonEmptyList.of(LifecycleHasEnded((c, s)).asInstanceOf[EE]).raiseError[F, S]
      else maybe((c, s))
    }
}

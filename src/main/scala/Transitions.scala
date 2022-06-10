package org.tp.process_time_state

import cats.data.Validated.{ Invalid, Valid }
import cats.data.*
import cats.implicits.*
import cats.{ ApplicativeError, FlatMap, Monad }

import scala.annotation.targetName

import Lifecycle.IsEnd

/** The Transitions[F] trait provides the Aggregate instance with defining state transitions and
  * invariants.
  */
trait Transitions[F[_]] { self: Aggregate[F] =>

  /** The (Command, State) input label */
  final type LabelIn = (C, S)

  /** We are accumulating domain errors in a NonEmptyList */
  final type NEC = NonEmptyChain[EE]

  /** A Transition is a partial function (not all combinations of commands and states need to have a
    * transition defined) that maps an input label to a state.
    */
  final type Transition = PartialFunction[LabelIn, F[S]]

  /** A TransitionF is just an alias to Kleisli[F, LabelIn, S]. Kleisli structures are
    * representations of functions A => F[B] - exactly what we need to model a function that maps a
    * (Command, State) pair to a new State within the context of F.
    */
  final type TransitionF = Kleisli[F, LabelIn, S]

  /** This abstract method needs to provide all the state transitions supported by the Aggregate.
    * You can use the `liftK` extension method on the Transition type to easily lift the composed
    * partial functions into a total Kleisli data structure.
    *
    * @see
    *   liftF
    */
  def transitions: TransitionF

  /** This error is indicated when there is no TransitionF for a LabelIn.
    */
  private final case class TransitionNotDefined(l: LabelIn) extends DomainError {
    override def msg: String = s"Transition is not defined for command ${l._1} and state ${l._2}"
  }

  /** This error indicates when the Aggregate is in a final state and can't process any more
    * commands
    */
  private final case class LifecycleHasEnded(l: LabelIn) extends DomainError {
    override def msg: String = s"Command ${l._1} cannot be processed because state ${l._2} is final"
  }

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
    /** Lifts a Transition into a TransitionF
      *
      * @param F
      *   An implicit ApplicativeError[F, NEC] instance
      * @param isFinal
      *   An implicit IsEnd[S] instance that determines if a state is final
      * @return
      *   The lifted TransitionF
      */
    final protected def liftF(using
        F: ApplicativeError[F, NEC],
        isFinal: IsEnd[S],
    ): TransitionF = Kleisli { (currentCommand: C, currentState: S) =>
      if isFinal(currentState) then
        NonEmptyChain
          .one(LifecycleHasEnded((currentCommand, currentState)).asInstanceOf[EE])
          .raiseError[F, S]
      else maybe((currentCommand, currentState))
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

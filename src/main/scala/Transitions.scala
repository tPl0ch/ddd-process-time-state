package org.tp
package process_time_state

import cats.ApplicativeError
import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ Kleisli, NonEmptyList, ValidatedNel }
import cats.implicits.*

import scala.annotation.targetName

trait Transitions[F[_]] { self: Aggregate[F] =>
  type Invariant   = PartialFunction[LabelIn, ValidatedNel[EE, Unit]]
  type Transition  = PartialFunction[LabelIn, F[S]]
  type TransitionF = Kleisli[F, LabelIn, S]

  def transitions: TransitionF

  final case class TransitionNotDefined(l: LabelIn) extends DomainError {
    override def msg: String = s"Transition is not defined for command ${l._1} and state ${l._2}"
  }

  final private def mkTransition(
      transitionToBeGuarded: Transition,
      guards: List[Invariant] = Nil,
  )(using
      applicativeError: ApplicativeError[F, NEL],
  ): Transition = (command: C, state: S) => {
    guards
      .filter(_.isDefinedAt((command, state)))
      .map(f => f((command, state)))
      .sequence match
      case Valid(_)          => transitionToBeGuarded((command, state))
      case Invalid(nel: NEL) => applicativeError.raiseError(nel)
  }

  final protected def mkTransitionF(
      transitionsToBeLifted: Transition,
  )(using
      applicativeError: ApplicativeError[F, NEL],
  ): TransitionF = Kleisli { (l: LabelIn) =>
    if !transitionsToBeLifted.isDefinedAt(l) then
      applicativeError.raiseError(NonEmptyList.of(TransitionNotDefined(l).asInstanceOf[EE]))
    else transitionsToBeLifted(l)
  }

  extension (transition: Transition)
    @targetName("withGuards")
    def <<<(
        invariants: List[Invariant],
    )(using
        applicativeError: ApplicativeError[F, NEL],
    ): Transition = mkTransition(
      transition,
      invariants,
    )

    @targetName("withGuard")
    def <<(
        invariant: Invariant,
    )(using
        applicativeError: ApplicativeError[F, NEL],
    ): Transition = mkTransition(
      transition,
      List(invariant),
    )
}

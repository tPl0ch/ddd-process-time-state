package org.tp
package process_time_state

import cats.ApplicativeError
import cats.data.Validated.{ Invalid, Valid }
import cats.data.ValidatedNel
import cats.implicits.*

import scala.annotation.targetName

trait Transitions[F[_]] { self: Aggregate[F] =>
  type Invariant   = PartialFunction[LabelIn, ValidatedNel[EE, Unit]]
  type TransitionF = PartialFunction[LabelIn, F[S]]

  def transitions: TransitionF

  final private def mkTransition(
      transitionToBeGuarded: TransitionF,
      guards: List[Invariant] = Nil,
  )(using
      applicativeError: ApplicativeError[F, NEC],
  ): TransitionF = {
    case (command: C, state: S) => {
      guards.map(f => f((command, state))).sequence match
        case Valid(_)          => transitionToBeGuarded((command, state))
        case Invalid(nel: NEC) => nel.raiseError[F, S]
    }
  }

  extension (transition: TransitionF)
    @targetName("withGuards")
    def <<<(
        invariants: List[Invariant],
    )(using
        applicativeError: ApplicativeError[F, NEC],
    ): TransitionF = mkTransition(
      transition,
      invariants,
    )

    @targetName("withGuard")
    def <<(
        invariant: Invariant,
    )(using
        applicativeError: ApplicativeError[F, NEC],
    ): TransitionF = mkTransition(
      transition,
      List(invariant),
    )
}

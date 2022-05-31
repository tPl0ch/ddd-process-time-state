package org.tp.process_time_state

import cats.data.Validated.*
import cats.data.{ NonEmptyChain, NonEmptyList, ValidatedNec }
import cats.{ Applicative, ApplicativeError, FlatMap, Functor, Id as CatsId }
import cats.implicits.*

object machines {

  type ErrorF[F[_]] = ApplicativeError[F, NonEmptyList[Throwable]]

  opaque type GuardF[C, S, E <: Throwable] = C => S => ValidatedNec[E, Unit]
  opaque type BehaviorF[F[_], C, S]        = C => S => F[S]

  object BehaviorF {
    def apply[F[_]: ErrorF, C, S, E <: Throwable](b: BehaviorF[F, C, S])(
        guards: List[GuardF[C, S, E]],
    ): BehaviorF[F, C, S] =
      (c: C) =>
        (s: S) => {
          guards.map(f => f(c)(s)).sequence match
            case Valid(_)                       => b(c)(s)
            case Invalid(nec: NonEmptyChain[E]) => nec.toNonEmptyList.raiseError[F, S]
        }
  }

  type OutputF[F[_], C, S, E] = C => S => F[E]

  opaque type StateF[F[_], S, E] = S => F[(S, E)]

  opaque type MachineF[F[_], C, S, E] = C => StateF[F, S, E]

  object StateF {
    def apply[F[_], S, E](f: S => F[(S, E)]): StateF[F, S, E] = f

    def apply[F[_]: FlatMap: Functor, S, E](
        behaviors: S => F[S],
    )(outputs: S => F[E]): StateF[F, S, E] =
      (currentState: S) =>
        FlatMap[F].flatMap(behaviors(currentState))(ss =>
          Functor[F].map(outputs(currentState))(e => (ss, e)),
        )
  }

  object MachineF {
    def apply[F[_]: FlatMap: Functor, C, S, E](behaviors: BehaviorF[F, C, S])(
        outputs: OutputF[F, C, S, E],
    ): MachineF[F, C, S, E] =
      (command: C) => StateF.apply(behaviors(command))(outputs(command))
  }
}

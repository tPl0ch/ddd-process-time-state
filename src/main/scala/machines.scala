package org.tp.process_time_state

import cats.data.Validated.*
import cats.data.{ NonEmptyChain, NonEmptyList, Validated, ValidatedNel }
import cats.{ Applicative, ApplicativeError, FlatMap, Functor, Id as CatsId }
import cats.implicits.*

import scala.annotation.targetName

type ErrorF[F[_]] = ApplicativeError[F, NonEmptyList[Throwable]]

type GuardF[C, S, E <: Throwable] = PartialFunction[(C, S), ValidatedNel[E, Unit]]
type BehaviorF[F[_], C, S]        = PartialFunction[(C, S), F[S]]
type OutputF[F[_], C, S, E]       = PartialFunction[(C, S), F[E]]

object BehaviorF {
  extension [F[_], C, S](behavior: BehaviorF[F, C, S])
    @targetName("withGuards")
    def <<[E <: Throwable](
        guards: List[GuardF[C, S, E]],
    )(using errorF: ErrorF[F]): BehaviorF[F, C, S] = BehaviorF(
      behavior,
      guards,
    )

  def apply[F[_]: ErrorF, C, S, E <: Throwable](
      behaviorToBeGuarded: BehaviorF[F, C, S],
      guards: List[GuardF[C, S, E]],
  ): BehaviorF[F, C, S] = {
    case (command: C, state: S) => {
      guards.map(f => f((command, state))).sequence match
        case Valid(_)                      => behaviorToBeGuarded((command, state))
        case Invalid(nel: NonEmptyList[E]) => nel.raiseError[F, S]
    }
  }
}

type StateF[F[_], S, E] = S => F[(S, E)]

object StateF {
  def apply[F[_], S, E](f: S => F[(S, E)]): StateF[F, S, E] = f

  def unit[F[_]: Functor, S, E](fe: F[E]): StateF[F, S, E] =
    s => Functor[F].map(fe)((s, _))

  extension [F[_], S, E](underlying: StateF[F, S, E]) def run(s: S): F[(S, E)] = underlying(s)
}
opaque type MachineF[F[_], C, S, E] = C => StateF[F, S, E]

object MachineF {
  def apply[F[_]: FlatMap, C, S, E](
      behaviors: BehaviorF[F, C, S],
      outputs: OutputF[F, C, S, E],
      initial: S,
  ): MachineF[F, C, S, E] =
    (command: C) =>
      StateF((_: S) =>
        for {
          s <- behaviors((command, initial))
          o <- outputs((command, initial))
        } yield (s, o),
      )
}

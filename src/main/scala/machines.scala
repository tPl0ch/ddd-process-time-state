package org.tp.process_time_state

import cats.data.Validated.*
import cats.data.{ NonEmptyChain, NonEmptyList, Validated, ValidatedNel }
import cats.{ Applicative, ApplicativeError, FlatMap, Functor }
import cats.implicits.*

import scala.annotation.targetName

type Label[C <: HasIdentity[ID], S <: HasIdentity[ID], ID] = (C, S)

type InvariantF[C <: HasIdentity[ID], S <: HasIdentity[ID], EE <: Throwable, ID] =
  PartialFunction[Label[C, S, ID], ValidatedNel[EE, Unit]]

type BehaviorF[F[_], C <: HasIdentity[ID], S <: HasIdentity[ID], ID] =
  PartialFunction[Label[C, S, ID], F[S]]

type OutputF[F[_], C <: HasIdentity[ID], S <: HasIdentity[ID], E, ID] =
  PartialFunction[Label[C, S, ID], F[E]]

type ErrorF[F[_]] = ApplicativeError[F, NonEmptyList[Throwable]]

object BehaviorF {
  extension [F[_], C <: HasIdentity[ID], S <: HasIdentity[ID], ID](behavior: BehaviorF[F, C, S, ID])
    @targetName("withGuards")
    def <<<[EE <: Throwable](
        invariants: List[InvariantF[C, S, EE, ID]],
    )(using errorF: ErrorF[F]): BehaviorF[F, C, S, ID] = BehaviorF(
      behavior,
      invariants,
    )

    @targetName("withGuard")
    def <<[EE <: Throwable](
        invariant: InvariantF[C, S, EE, ID],
    )(using errorF: ErrorF[F]): BehaviorF[F, C, S, ID] = BehaviorF(
      behavior,
      List(invariant),
    )

  def apply[F[_]: ErrorF, C <: HasIdentity[ID], S <: HasIdentity[ID], ID, EE <: Throwable](
      behaviorToBeGuarded: BehaviorF[F, C, S, ID],
      guards: List[InvariantF[C, S, EE, ID]],
  ): BehaviorF[F, C, S, ID] = {
    case (command: C, state: S) => {
      guards.map(f => f((command, state))).sequence match
        case Valid(_)                       => behaviorToBeGuarded((command, state))
        case Invalid(nel: NonEmptyList[EE]) => nel.raiseError[F, S]
    }
  }
}

type StateF[F[_], C <: HasIdentity[ID], S <: HasIdentity[ID], E <: HasIdentity[ID], ID] =
  Label[C, S, ID] => F[(S, E)]

object StateF {
  def apply[F[_]: FlatMap, C <: HasIdentity[ID], S <: HasIdentity[ID], E <: HasIdentity[ID], ID](
      behaviors: BehaviorF[F, C, S, ID],
      outputs: OutputF[F, C, S, E, ID],
  ): StateF[F, C, S, E, ID] =
    (currentCommand: C, currentState: S) =>
      for {
        newState     <- behaviors((currentCommand, currentState))
        outputSignal <- outputs((currentCommand, currentState))
      } yield (newState, outputSignal)

  extension [F[_], C <: HasIdentity[ID], S <: HasIdentity[ID], E <: HasIdentity[ID], ID](
      underlying: StateF[F, C, S, E, ID],
  ) def run(l: Label[C, S, ID]): F[(S, E)] = underlying(l)
}

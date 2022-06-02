package org.tp.process_time_state

import cats.data.Validated.*
import cats.data.{ NonEmptyChain, NonEmptyList, Validated, ValidatedNel }
import cats.{ Applicative, ApplicativeError, FlatMap, Functor, Id as CatsId }
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

type StateF[F[_], S, E] = S => F[(S, E)]

object StateF {
  def apply[F[_], S, E](f: S => F[(S, E)]): StateF[F, S, E] = f

  def unit[F[_]: Functor, S, E](fe: F[E]): StateF[F, S, E] =
    s => Functor[F].map(fe)((s, _))

  extension [F[_], S, E](underlying: StateF[F, S, E]) def run(s: S): F[(S, E)] = underlying(s)
}

type MachineF[F[_], C, S, E] = C => StateF[F, S, E]

object MachineF {
  def apply[F[_]: FlatMap, C <: HasIdentity[ID], S <: HasIdentity[ID], E, ID](
      behaviors: BehaviorF[F, C, S, ID],
      outputs: OutputF[F, C, S, E, ID],
  ): MachineF[F, C, S, E] =
    (command: C) =>
      StateF((currentState: S) =>
        for {
          newState     <- behaviors((command, currentState))
          outputSignal <- outputs((command, currentState))
        } yield (newState, outputSignal),
      )
}

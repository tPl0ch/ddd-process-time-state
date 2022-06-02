package org.tp
package process_time_state

import cats.data.Validated.*
import cats.data.{ NonEmptyChain, NonEmptyList, Validated, ValidatedNel }
import cats.{ Applicative, ApplicativeError, FlatMap, Functor }
import cats.implicits.*

import scala.annotation.targetName

trait Aggregate[F[_]] {
  type ID
  type C <: HasIdentity[ID]
  type S <: HasIdentity[ID]
  type E <: HasIdentity[ID]
  type EE <: DomainError
  type NEC = NonEmptyList[EE]

  type LabelIn   = (C, S)
  type LabelOutF = F[(S, E)]

  type Invariant   = PartialFunction[LabelIn, ValidatedNel[EE, Unit]]
  type TransitionF = PartialFunction[LabelIn, F[S]]
  type EventF      = PartialFunction[LabelIn, F[E]]

  type StateF = LabelIn => LabelOutF

  def transitions: TransitionF
  def events: EventF

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

  final protected def mkStateF(
      transitions: TransitionF,
      events: EventF,
  )(using flatMap: FlatMap[F]): StateF =
    (currentCommand: C, currentState: S) =>
      for {
        newState     <- transitions((currentCommand, currentState))
        outputSignal <- events((currentCommand, currentState))
      } yield (newState, outputSignal)

  extension (underlying: StateF) def run(l: LabelIn): LabelOutF = underlying(l)
}

package org.tp.process_time_state

import identity.*

import cats.ApplicativeError
import cats.data.NonEmptyList

trait Aggregate[F[_]]
    extends Lifecycles[F]
    with Identities[F]
    with Transitions[F]
    with Events[F]
    with States[F] {

  /** The Command alphabet type as a subtype of HasIdentity[ID] */
  type C <: HasIdentity[ID]

  /** The State alphabet type as a subtype of HasIdentity[ID] */
  type S <: HasIdentity[ID]

  /** The Event alphabet type as a subtype of HasIdentity[ID] */
  type E <: HasIdentity[ID]

  /** The Domain Error type as a subtype of DomainError */
  type EE <: DomainError

  /** We are accumulating domain errors in a NonEmptyList */
  type NEL = NonEmptyList[EE]

  /** The (Command, State) input label */
  final type LabelIn = (C, S)

  /** The (State, Event) output label */
  final type LabelOut = (S, E)

  /** The (State, Event) output label lifted into Effect F */
  final type LabelOutF = F[LabelOut]
}

object Aggregate {
  def maybe[F[_], IN, OUT, EE <: DomainError](
      partialFunction: PartialFunction[IN, F[OUT]],
      e: IN => EE,
  )(using ae: ApplicativeError[F, NonEmptyList[EE]]): PartialFunction[IN, F[OUT]] =
    (in: IN) =>
      if !partialFunction.isDefinedAt(in) then ae.raiseError(NonEmptyList.of(e(in)))
      else partialFunction(in)
}

package org.tp.process_time_state

import identity.*

import cats.data.NonEmptyList

// TODO: traverse for List[C]
trait Aggregate[F[_]] extends Identities[F] with Transitions[F] with Events[F] with States[F] {

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
  type LabelIn = (C, S)

  /** The (State, Event) output label lifted into Effect F */
  type LabelOutF = F[(S, E)]
}

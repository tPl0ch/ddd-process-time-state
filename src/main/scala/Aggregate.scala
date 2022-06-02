package org.tp
package process_time_state

import cats.data.NonEmptyList

// TODO: traverse for List[C]
trait Aggregate[F[_]] extends Transitions[F] with Events[F] with States[F] {

  /** The type of the Aggregate's identity */
  type ID

  /** The Command alphabet type as a subtype of HasIdentity[ID] */
  type C <: HasIdentity[ID]

  /** The State alphabet type as a subtype of HasIdentity[ID] */
  type S <: HasIdentity[ID]

  /** The Event alphabet type as a subtype of HasIdentity[ID] */
  type E <: HasIdentity[ID]

  /** The Domain Error type as a subtype of DomainError */
  type EE <: DomainError

  /** We are accumulating domain errors in a NonEmptyList */
  type NEC = NonEmptyList[EE]

  /** The (Command, State) input label */
  type LabelIn = (C, S)

  /** The (State, Event) output label lifted into Effect F */
  type LabelOutF = F[(S, E)]
}

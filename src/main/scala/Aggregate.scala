package org.tp.process_time_state

import identity.*

trait Aggregate[F[_]] extends Lifecycles[F] with Identities[F] with Transitions[F] {

  /** The Command alphabet type as a subtype of HasIdentity[ID] */
  type C <: HasIdentity[ID]

  /** The State alphabet type as a subtype of HasIdentity[ID] */
  type S <: HasIdentity[ID]

  /** The Domain Error type as a subtype of DomainError */
  type EE <: DomainError
}

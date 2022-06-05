package org.tp.process_time_state

import Lifecycle.PreGenesis
import identity.*

import cats.data.ValidatedNel
import cats.implicits.*

trait Identities[F[_]] { self: Aggregate[F] =>

  /** The type of the Aggregate's identity */
  type ID

  /** This is the error representation when two Aggregate IDs are not matching.
    *
    * @param thisId
    *   The lhs ID.
    * @param otherId
    *   The rhs ID.
    */
  final case class IdentitiesDoNotMatch(
      thisId: ID | PreGenesis.type,
      otherId: ID | PreGenesis.type,
  ) extends DomainError {
    def msg: String = s"Identities $thisId and $otherId do not match"
  }

  /** This factory function lets you create an identity guard for a specific behavior.
    *
    * @param equalIdentities
    *   The EqualIdentities[ID, GENESIS] implicit type-class
    * @return
    *   An InvariantF built from the provided types
    */
  final protected def identityInvariant(using
      equalIdentities: EqualIdentities[ID],
  ): Invariant = { case (c: C, s: S) =>
    if equalIdentities.equals(c.id, s.id) then ().validNel
    else IdentitiesDoNotMatch(c.id, s.id).asInstanceOf[EE].invalidNel
  }
}

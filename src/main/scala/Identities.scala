package org.tp.process_time_state

import cats.data.ValidatedNec
import cats.implicits.*

import Lifecycle.PreGenesis
import identity.*

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
  final case class IdentitiesDoNotMatch(thisId: UID[ID], otherId: UID[ID]) extends DomainError {
    def msg: String = s"Identities $thisId and $otherId do not match"
  }

  /** This factory function lets you create an identity guard for a specific behavior.
    *
    * @param equalIdentities
    *   The EqualIdentities[ID] implicit type-class
    * @return
    *   An InvariantF built from the provided types
    */
  final protected def identitiesMustMatch(using
      equalIdentities: EqualIdentities[ID],
  ): Invariant = { case (c: C, s: S) =>
    if equalIdentities.equals(c.id, s.id) then ().validNec
    else IdentitiesDoNotMatch(c.id, s.id).asInstanceOf[EE].invalidNec
  }
}

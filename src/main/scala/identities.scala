package org.tp
package process_time_state

import cats.data.ValidatedNel
import cats.syntax.validated.*

/** This object defines the pre-genesis state, i.e. a potential customer.
  */
case object NoIdentitySet

/** This trait needs to be implemented by the Command, State and Even alphabets and provides the
  * Aggregate identity. This identity can be defined from the the outside and needs to be
  * accompanied by an EqualIdentities[ID] given instance if you want to leverage the identity guard.
  *
  * An implementer can decide if a concrete ID type or the NoIdentitySet singleton should be
  * returned.
  *
  * @tparam ID
  *   The Aggregate ID type.
  */
trait HasIdentity[ID] {
  def id: ID | NoIdentitySet.type
}

/** This trait is a comparison type-class for a specific Aggregate ID that you have chosen.
  * @tparam ID
  *   The Aggregate ID type.
  */
trait EqualIdentities[ID] {
  def equals(idA: ID | NoIdentitySet.type, idB: ID | NoIdentitySet.type): Boolean
}

/** This is the error representation when two Aggregate IDs are not matching.
  *
  * @param thisId
  *   The lhs ID.
  * @param otherId
  *   The rhs ID.
  * @tparam ID
  *   The Aggregate ID type.
  */
final case class IdentitiesDoNotMatch[ID](
    thisId: ID | NoIdentitySet.type,
    otherId: ID | NoIdentitySet.type,
) extends DomainError {
  def msg: String = s"Identities $thisId and $otherId do not match"
}

/** This factory function lets you create an identity guard for a specific behavior.
  *
  * @param equalIdentities
  *   The EqualIdentities[ID] implicit type-class
  * @tparam C
  *   The Command type.
  * @tparam S
  *   The State type.
  * @tparam ID
  *   The Aggregate ID type.
  * @return
  *   An InvariantF built from the provided types
  */
def identityGuard[C <: HasIdentity[ID], S <: HasIdentity[ID], ID](using
    equalIdentities: EqualIdentities[ID],
): PartialFunction[(C, S), ValidatedNel[DomainError, Unit]] = { case (c: C, s: S) =>
  if equalIdentities.equals(c.id, s.id) then ().validNel
  else IdentitiesDoNotMatch(c.id, s.id).invalidNel
}

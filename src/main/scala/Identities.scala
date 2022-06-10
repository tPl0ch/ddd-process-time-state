package org.tp.process_time_state

import cats.syntax.validated.*

import Lifecycle.NoId
import identity.*

object Identities {

  /** The UID[ID] type represents a Union of the Aggregate's concrete ID type and the PreGenesis
    * state.
    */
  type UID[ID] = ID | NoId.type

  /** This is the error representation when two Aggregate IDs are not matching.
    *
    * @param thisId
    *   The lhs ID.
    * @param otherId
    *   The rhs ID.
    */
  case class IdentitiesDoNotMatch[ID](thisId: UID[ID], otherId: UID[ID]) extends Error {
    override def msg: String = s"Identities $thisId and $otherId do not match"
  }

  def identitiesMustMatch[ID, C <: HasId[ID], S <: HasId[ID], EE <: Error](using
      equalIdentities: EqualId[ID],
  ): Invariant[C, S, EE] = { case (c: C, s: S) =>
    if equalIdentities.equals(c.id, s.id) then ().validNec
    else IdentitiesDoNotMatch(c.id, s.id).asInstanceOf[EE].invalidNec
  }

}

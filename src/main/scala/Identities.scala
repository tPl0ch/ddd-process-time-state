package org.tp.process_time_state

import cats.syntax.validated.*

import Lifecycle.NoId
import identity.*

object Identities {

  /** This is the error representation when two Aggregate IDs are not matching.
    *
    * @param thisId
    *   The lhs ID.
    * @param otherId
    *   The rhs ID.
    */
  case class IdentitiesDoNotMatch[ID](thisId: UID[ID], otherId: UID[ID]) extends DomainError {
    def msg: String = s"Identities $thisId and $otherId do not match"
  }

  def identitiesMustMatch[ID, C <: HasId[ID], S <: HasId[ID], EE <: Throwable](using
      equalIdentities: EqualId[ID],
  ): Invariant[C, S, EE] = { case (c: C, s: S) =>
    if equalIdentities.equals(c.id, s.id) then ().validNec
    else IdentitiesDoNotMatch(c.id, s.id).asInstanceOf[EE].invalidNec
  }

}

package org.tp.process_time_state

import cats.syntax.validated.*

import Lifecycles.*

object Identities {

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

  def identitiesMustMatch[ID, C <: Lifecycle[ID], S <: Lifecycle[ID], EE <: Error](using
      equalIdentities: ComparesLifecycles[ID],
  ): Invariant[C, S, EE] = { case (c: C, s: S) =>
    if equalIdentities.equals(c, s) then ().validNec
    else IdentitiesDoNotMatch(c, s).asInstanceOf[EE].invalidNec
  }

}

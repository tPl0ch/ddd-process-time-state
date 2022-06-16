package org.tp.process_time_state

import cats.kernel.Eq
import cats.data.{ NonEmptyChain, Validated }
import cats.syntax.validated.*

type InvariantError[+EE <: Throwable, A] =
  Validated[NonEmptyChain[EE], A]

type Invariant[-C, -S, +EE <: Error] =
  PartialFunction[(C, S), InvariantError[EE, ?]]

object Invariants {
  def identitiesMustMatch[ID, C <: Lifecycle[ID], S <: Lifecycle[ID], EE <: Error](using
      EQ: Eq[Lifecycle[ID]],
  ): Invariant[C, S, EE] = { case (c: C, s: S) =>
    if EQ.eqv(c, s) then ().validNec
    else IdentitiesDoNotMatch(c, s).asInstanceOf[EE].invalidNec
  }

  extension [C, S, EE <: Error](invariant: Invariant[C, S, EE])
    def check: Invariant[C, S, EE] =
      (c: C, s: S) =>
        if !invariant.isDefinedAt((c, s)) then ().validNec
        else invariant((c, s))

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
}

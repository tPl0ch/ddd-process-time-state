package org.tp.process_time_state

import cats.data.{ NonEmptyChain, ValidatedNec }
import cats.syntax.validated.*

import scala.annotation.targetName

import identity.{ EqualId, HasId }

type InvariantError[+EE <: Throwable, A] = ValidatedNec[EE, A]
type Invariant[-C, -S, +EE <: Throwable] = PartialFunction[(C, S), InvariantError[EE, Unit]]

object Invariants {

  extension [C, S, EE <: Throwable](invariant: Invariant[C, S, EE])
    def check: Invariant[C, S, EE] =
      (c: C, s: S) =>
        if !invariant.isDefinedAt((c, s)) then ().validNec
        else invariant((c, s))
}

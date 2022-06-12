package org.tp.process_time_state

import cats.data.{ NonEmptyChain, Validated }
import cats.syntax.validated.*

type InvariantError[+EE <: Throwable, A] =
  Validated[NonEmptyChain[EE], A]

type Invariant[-C, -S, +EE <: Error] =
  PartialFunction[(C, S), InvariantError[EE, ?]]

object Invariants {
  extension [C, S, EE <: Error](invariant: Invariant[C, S, EE])
    def check: Invariant[C, S, EE] =
      (c: C, s: S) =>
        if !invariant.isDefinedAt((c, s)) then ().validNec
        else invariant((c, s))
}

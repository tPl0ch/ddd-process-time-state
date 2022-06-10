package org.tp.process_time_state

import cats.data.Validated
import cats.syntax.validated.*

import scala.annotation.targetName

trait Invariants[F[_]] { self: Aggregate[F] =>

  /** We use the accumulating Validated structure */
  final type InvariantError = Validated[NEC, Unit]

  /** An Invariant is a partial function that either gives a valid Unit value, or an accumulating
    * error structure.
    */
  final type Invariant = PartialFunction[LabelIn, InvariantError]

  /** This error is indicated when there is no Invariant for a LabelIn on a specific TransitionF */
  private final case class InvariantCannotBeChecked(l: LabelIn) extends DomainError {
    override def msg: String =
      s"An Invariant could not be checked for command ${l._1} and state ${l._2}"
  }

  extension (invariant: Invariant)
    @targetName("maybeInvariant")
    final protected def maybe: Invariant =
      (l: LabelIn) =>
        if !invariant.isDefinedAt(l) then ().validNec
        else invariant(l)

}

package org.tp.process_time_state

import cats.data.Kleisli
import cats.implicits.*
import cats.{ ApplicativeError, FlatMap, Monad }

import scala.annotation.targetName

import identity.HasId

/** This trait provides the Event output types and functions */
trait Events[F[_], C, S, E] {

  /** Provides the output Events for the Aggregate
    *
    * @return
    *   The lifted partial output function that provides the Aggregate's Events within the context F
    */
  def events: TransitionK[F, C, S, E]
}

object Events {

  /** This error indicates when there is no Event defined for a valid Transition */
  final case class EventNotDefined[C, S](c: C, s: S) extends DomainError {
    override def msg: String = s"Event for input label ($c, $s) is not defined."
  }
}

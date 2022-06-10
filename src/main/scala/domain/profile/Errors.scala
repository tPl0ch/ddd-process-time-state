package org.tp.process_time_state
package domain.profile

import domain.profile.Types.*

object Errors {
  sealed trait ProfileError extends Error

  /** This error is indicated when there is no TransitionF for a LabelIn.
    */
  final case class TransitionNotDefined(c: C, s: S) extends ProfileError {
    override def msg: String = s"Transition is not defined for command $c and state $s"
  }

  /** This error indicates when there is no Event defined for a valid Transition */
  final case class EventNotDefined[C, S](c: C, s: S) extends ProfileError {
    override def msg: String = s"Event for input label ($c, $s) is not defined."
  }
}

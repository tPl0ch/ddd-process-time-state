package org.tp.process_time_state
package domain.registration

import domain.registration.Model.Token
import domain.registration.Types.*

object Errors {
  sealed trait AccountRegistrationError extends Error

  final case class InvalidToken(token: Token) extends AccountRegistrationError {
    override def msg: String = s"Token '${token.value}' is invalid"
  }

  /** This error is indicated when there is no TransitionF for a LabelIn.
    */
  final case class TransitionNotDefined(c: C, s: S) extends AccountRegistrationError {
    override def msg: String = s"Transition is not defined for command $c and state $s"
  }

  /** This error indicates when there is no Event defined for a valid Transition */
  final case class EventNotDefined[C, S](c: C, s: S) extends AccountRegistrationError {
    override def msg: String = s"Event for input label ($c, $s) is not defined."
  }
}

package org.tp.process_time_state
package domain.registration

import domain.registration.Model.Token
import domain.registration.Types.*

object Errors {
  sealed trait RegistrationError extends Error

  final case class InvalidToken(token: Token) extends RegistrationError {
    override def msg: String = s"Token '${token.value}' is invalid"
  }
}

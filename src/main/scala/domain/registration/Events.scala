package org.tp.process_time_state
package domain.registration

import cats.syntax.validated.*

import Transitions.*
import domain.registration.Behaviors.*
import domain.registration.Givens.given
import domain.registration.Model.{ Command, Event, State }
import domain.registration.Types.*

object Events {
  def events: OutputsK[EIO, C, S, E] = (registrationStarted orElse emailConfirmed).liftF

  val registrationStarted: Output[C, S, E] = {
    case (c: Command.StartRegistration, _: State.PotentialCustomer) =>
      Event.RegistrationStarted(c.id, c.email, c.token)
  }

  val emailConfirmed: Output[C, S, E] = {
    case (_: Command.ConfirmEmail, s: State.WaitingForEmailRegistration) =>
      Event.EmailConfirmed(s.id, s.email)
  }
}

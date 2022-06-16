package org.tp.process_time_state
package domain.registration

import Behaviors.*
import Givens.given
import Model.{ Command, Event, State }
import Types.*

object Events {
  def events: OutputsK[EIO, C, S, E] = (registrationStarted orElse emailConfirmed).liftF

  val registrationStarted: RegistrationOutput = {
    case (c: Command.StartRegistration, _: State.PotentialCustomer) =>
      Event.RegistrationStarted(c.id, c.email, c.token)
  }

  val emailConfirmed: RegistrationOutput = {
    case (_: Command.ConfirmEmail, s: State.WaitingForEmailRegistration) =>
      Event.EmailConfirmed(s.id, s.email)
  }
}

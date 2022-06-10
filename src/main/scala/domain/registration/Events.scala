package org.tp.process_time_state
package domain.registration

import cats.syntax.validated.*

import Transitions.*
import domain.registration.Behaviors.*
import domain.registration.Errors.EventNotDefined
import domain.registration.Givens.given
import domain.registration.Model.{ Command, Event, State }
import domain.registration.Types.*

object Events {
  def events: TransitionK[EIO, C, S, E] = (registrationStarted orElse emailConfirmed).liftF

  val registrationStarted: Transition[C, S, E, EE] = {
    case (c: Command.StartRegistration, _: State.PotentialCustomer) =>
      Event.RegistrationStarted(c.id, c.email, c.token).validNec
  }

  val emailConfirmed: Transition[C, S, E, EE] = {
    case (_: Command.ConfirmEmail, s: State.WaitingForEmailRegistration) =>
      Event.EmailConfirmed(s.id, s.email).validNec
  }

  private implicit val eventsError: (C, S) => EE = (c: C, s: S) => EventNotDefined(c, s)
}

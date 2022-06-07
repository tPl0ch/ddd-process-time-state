package org.tp.process_time_state
package impl

import UserRegistration.Commands.{ ConfirmEmail, StartRegistration }
import UserRegistration.States.{ PotentialCustomer, WaitingForEmailRegistration }
import UserRegistration.{ Email, Token, UserId }
import identity.HasIdentity

import cats.implicits.*

final class UserRegistrationWithEvents extends UserRegistration with Events[ErrorOr] {
  import UserRegistrationWithEvents.Events.*

  final override type E = UserRegistrationWithEvents.Events

  override def events: OutputsF = (registrationStarted orElse emailConfirmed).liftK

  val registrationStarted: Outputs = { case (c: StartRegistration, _: PotentialCustomer) =>
    RegistrationStarted(c.id, c.email, c.token).asRight
  }

  val emailConfirmed: Outputs = { case (_: ConfirmEmail, s: WaitingForEmailRegistration) =>
    EmailConfirmed(s.id, s.email).asRight
  }
}

object UserRegistrationWithEvents {
  enum Events extends HasIdentity[UserId]:
    case RegistrationStarted(id: UserId, email: Email, token: Token)
    case EmailConfirmed(id: UserId, email: Email)
    case GDPRDeleted(id: UserId)
}

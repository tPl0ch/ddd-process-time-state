package org.tp.process_time_state
package impl

import AccountRegistration.Commands.{ ConfirmEmail, StartRegistration }
import AccountRegistration.States.{ PotentialCustomer, WaitingForEmailRegistration }
import AccountRegistration.{ AccountId, Email, Token }
import identity.HasIdentity

import cats.implicits.*

final class AccountRegistrationWithEvents extends AccountRegistration with Events[ErrorOr] {
  import AccountRegistrationWithEvents.Events.*

  final override type E = AccountRegistrationWithEvents.Events

  override def events: OutputsF = (registrationStarted orElse emailConfirmed).liftK

  val registrationStarted: Outputs = { case (c: StartRegistration, _: PotentialCustomer) =>
    RegistrationStarted(c.id, c.email, c.token).asRight
  }

  val emailConfirmed: Outputs = { case (_: ConfirmEmail, s: WaitingForEmailRegistration) =>
    EmailConfirmed(s.id, s.email).asRight
  }
}

object AccountRegistrationWithEvents {
  enum Events extends HasIdentity[AccountId]:
    case RegistrationStarted(id: AccountId, email: Email, token: Token)
    case EmailConfirmed(id: AccountId, email: Email)
    case GDPRDeleted(id: AccountId)
}

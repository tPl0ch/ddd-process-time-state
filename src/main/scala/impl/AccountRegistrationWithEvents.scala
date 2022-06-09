package org.tp.process_time_state
package impl

import identity.HasIdentity
import impl.AccountRegistration.Commands.{ ConfirmEmail, StartRegistration }
import impl.AccountRegistration.States.{ PotentialCustomer, WaitingForEmailRegistration }
import impl.AccountRegistration.{ AccountId, Email, Token }

import cats.data.EitherT
import cats.implicits.*

import scala.concurrent.Future

final class AccountRegistrationWithEvents(using ec: scala.concurrent.ExecutionContext)
    extends AccountRegistration
    with Events[ErrorOr] {
  import AccountRegistrationWithEvents.Events.*

  final override type E = AccountRegistrationWithEvents.Events

  override def events: OutputsF = (registrationStarted orElse emailConfirmed).liftK

  val registrationStarted: Outputs = { case (c: StartRegistration, _: PotentialCustomer) =>
    EitherT.apply(Future.successful(RegistrationStarted(c.id, c.email, c.token).asRight))
  }

  val emailConfirmed: Outputs = { case (_: ConfirmEmail, s: WaitingForEmailRegistration) =>
    EitherT.apply(Future.successful(EmailConfirmed(s.id, s.email).asRight))
  }
}

object AccountRegistrationWithEvents {
  enum Events extends HasIdentity[AccountId]:
    case RegistrationStarted(id: AccountId, email: Email, token: Token)
    case EmailConfirmed(id: AccountId, email: Email)
    case GDPRDeleted(id: AccountId)
}

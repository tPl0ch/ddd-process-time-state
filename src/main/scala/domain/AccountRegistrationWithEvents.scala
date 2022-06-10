package org.tp.process_time_state
package domain

import cats.data.EitherT
import cats.effect.IO
import cats.implicits.*

import scala.concurrent.Future

import domain.AccountRegistration.Commands.{ ConfirmEmail, StartRegistration }
import domain.AccountRegistration.States.{ PotentialCustomer, WaitingForEmailRegistration }
import domain.AccountRegistration.{ AccountId, Email, Token }
import identity.HasIdentity

final class AccountRegistrationWithEvents
    extends AccountRegistration
    with Events[RegistrationEither] {
  import AccountRegistrationWithEvents.Events.*

  final override type E = AccountRegistrationWithEvents.Events

  override def events: OutputsF = (registrationStarted orElse emailConfirmed).liftF

  val registrationStarted: Outputs = { case (c: StartRegistration, _: PotentialCustomer) =>
    EitherT(IO.pure(RegistrationStarted(c.id, c.email, c.token).asRight))
  }

  val emailConfirmed: Outputs = { case (_: ConfirmEmail, s: WaitingForEmailRegistration) =>
    EitherT(IO.pure(EmailConfirmed(s.id, s.email).asRight))
  }
}

object AccountRegistrationWithEvents {
  enum Events extends HasIdentity[AccountId]:
    case RegistrationStarted(id: AccountId, email: Email, token: Token)
    case EmailConfirmed(id: AccountId, email: Email)
    case GDPRDeleted(id: AccountId)
}

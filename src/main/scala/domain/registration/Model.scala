package org.tp.process_time_state
package domain.registration

import java.util.UUID

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO

import Lifecycles.*

object Model {

  final case class AccountId(id: UUID)  extends AnyVal
  final case class Email(value: String) extends AnyVal
  final case class Token(value: String) extends AnyVal

  enum Command extends Lifecycle[AccountId]:
    case StartRegistration(id: AccountId, email: Email, token: Token)
    case ConfirmEmail(id: AccountId, token: Token)
    case RestartRegistration(id: AccountId)
    case DeleteDueToGDPR(id: AccountId)

  enum State extends Lifecycle[AccountId]:
    case PotentialCustomer(id: NotStarted.type = NotStarted)
    case WaitingForEmailRegistration(
        id: AccountId,
        email: Email,
        token: Token,
    )
    case Active(id: AccountId, email: Email)
    case Deleted(id: AccountId)

  enum Event extends Lifecycle[AccountId]:
    case RegistrationStarted(id: AccountId, email: Email, token: Token)
    case EmailConfirmed(id: AccountId, email: Email)
    case GDPRDeleted(id: AccountId)
}

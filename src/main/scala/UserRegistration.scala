package org.tp
package process_time_state

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.validated.*
import cats.implicits.*
import cats.instances.either.*

import java.util.UUID

type ErrorOr[A] = Either[NonEmptyList[DomainError], A]

final class UserRegistration extends Aggregate[ErrorOr] {
  import UserRegistration.*

  import Commands.*
  import States.*
  import Events.*

  import givens.userIdEquals

  override type ID = UserId
  override type C  = Commands
  override type S  = States
  override type E  = Events
  override type EE = DomainError

  override def transitions: TransitionF =
    (registerAction orElse
      (confirmationTransition << confirmationGuard)) << identityGuard

  override def events: EventF =
    registerEvent orElse confirmationEvent

  type RegistrationBehavior = UserRegistration#TransitionF
  type RegistrationGuard    = UserRegistration#Invariant
  type RegistrationEvent    = UserRegistration#EventF

  val registerAction: RegistrationBehavior = { case (c: Register, _: PotentialCustomer) =>
    AwaitingRegistrationConfirmation(c.id, c.email, c.token).asRight
  }

  val registerEvent: RegistrationEvent = { case (c: Register, _: PotentialCustomer) =>
    NewConfirmationRequested(c.id, c.email, c.token).asRight
  }

  final case class InvalidToken(token: Token) extends UserRegistrationError {
    override def msg: String = s"Token '${token.value}' is invalid"
  }

  val confirmationGuard: RegistrationGuard = {
    case (c: Confirm, s: AwaitingRegistrationConfirmation) =>
      if c.token.value != s.token.value then InvalidToken(c.token).invalidNel else ().validNel
  }

  val confirmationTransition: RegistrationBehavior = {
    case (_: Confirm, s: AwaitingRegistrationConfirmation) =>
      Active(s.id, s.email).asRight
  }

  val confirmationEvent: RegistrationEvent = {
    case (_: Confirm, s: AwaitingRegistrationConfirmation) =>
      Registered(s.id, s.email).asRight
  }
}

object UserRegistration {

  def apply(): UserRegistration = new UserRegistration

  sealed trait UserRegistrationError extends DomainError

  final case class UserId(id: UUID)     extends AnyVal
  final case class Email(value: String) extends AnyVal
  final case class Token(value: String) extends AnyVal

  enum Commands extends HasIdentity[UserId]:
    case Register(id: UserId, email: Email, token: Token)
    case Confirm(id: UserId, token: Token)
    case ResendConfirmation(id: UserId)
    case GDPRDeletion(id: UserId)

  enum Events extends HasIdentity[UserId]:
    case Registered(id: UserId, email: Email)
    case NewConfirmationRequested(id: UserId, email: Email, token: Token)
    case GDPRDeleted(id: UserId)

  enum States extends HasIdentity[UserId]:
    case PotentialCustomer(id: NoIdentitySet.type = NoIdentitySet)
    case AwaitingRegistrationConfirmation(
        id: UserId,
        email: Email,
        token: Token,
    )
    case Active(id: UserId, email: Email)
    case Deleted(id: UserId)

  object givens {
    given userIdEquals: EqualIdentities[UserRegistration#ID] with
      override def equals(
          idA: UserId | NoIdentitySet.type,
          idB: UserId | NoIdentitySet.type,
      ): Boolean =
        (idA, idB) match
          case (a: UserId, b: UserId)     => a.id.equals(b.id)
          case (_: NoIdentitySet.type, _) => false
          case (_, _: NoIdentitySet.type) => true
          case _                          => false
  }
}

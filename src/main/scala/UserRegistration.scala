package org.tp.process_time_state

import Lifecycle.*
import UserRegistration.UserRegistrationError
import identity.*

import cats.data.{ Kleisli, NonEmptyList }
import cats.implicits.*
import cats.instances.either.*
import cats.syntax.either.*
import cats.syntax.validated.*

import java.util.UUID

type ErrorOr[A] = Either[NonEmptyList[UserRegistrationError], A]

final class UserRegistration extends Aggregate[ErrorOr] {
  import UserRegistration.*

  import Commands.*
  import Events.*
  import States.*
  import givens.{ isFinalState, userIdEquals }

  override type ID = UserId
  override type C  = Commands
  override type S  = States
  override type E  = Events
  override type EE = UserRegistrationError

  override def transitions: TransitionF = mkTransitionF(
    (registerTransition orElse
      (confirmTransition << confirmGuard)) << identityInvariant,
  )

  override def events: EventF = mkEventsF(registerEvent orElse confirmEvent)

  val registerTransition: Transition =
    case (c: Register, _: PotentialCustomer) =>
      AwaitingRegistrationConfirmation(c.id, c.email, c.token).asRight

  val registerEvent: Event = { case (c: Register, _: PotentialCustomer) =>
    NewConfirmationRequested(c.id, c.email, c.token).asRight
  }

  final case class InvalidToken(token: Token) extends UserRegistrationError {
    override def msg: String = s"Token '${token.value}' is invalid"
  }

  val confirmGuard: Invariant = { case (c: Confirm, s: AwaitingRegistrationConfirmation) =>
    if c.token.value != s.token.value then InvalidToken(c.token).invalidNel else ().validNel
  }

  val confirmTransition: Transition =
    case (_: Confirm, s: AwaitingRegistrationConfirmation) =>
      Active(s.id, s.email).asRight

  val confirmEvent: Event = { case (_: Confirm, s: AwaitingRegistrationConfirmation) =>
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
    case PotentialCustomer(id: PreGenesis.type = PreGenesis)
    case AwaitingRegistrationConfirmation(
        id: UserId,
        email: Email,
        token: Token,
    )
    case Active(id: UserId, email: Email)
    case Deleted(id: UserId)

  object givens {
    given isFinalState: IsOmega[States] with
      override def apply(s: States): Boolean = s match
        case _: States.Deleted => true
        case _                 => false

    given userIdEquals: EqualIdentities[UserId] with
      override def equals(
          idA: UserId | PreGenesis.type,
          idB: UserId | PreGenesis.type,
      ): Boolean =
        (idA, idB) match
          case (a: UserId, b: UserId)  => a.id.equals(b.id)
          case (_: PreGenesis.type, _) => false // Commands should always have an identity
          case (_, _: PreGenesis.type) => true  // If there is a pre-genesis state, allow
          case _                       => false
  }
}

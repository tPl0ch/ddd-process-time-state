package org.tp.process_time_state

import Lifecycle.*
import UserRegistration.Commands.*
import UserRegistration.States.*
import UserRegistration.*
import UserRegistration.givens.given
import identity.*
import impl.{ SimpleUserRegistration, UserRegistrationWithEvents }

import cats.data.{ Kleisli, NonEmptyChain }
import cats.implicits.*
import cats.instances.either.*
import cats.syntax.either.*
import cats.syntax.validated.*

import java.util.UUID

type ErrorOr[A] = Either[NonEmptyChain[UserRegistrationError], A]

abstract class UserRegistration extends Aggregate[ErrorOr] {

  final override type ID = UserId
  final override type C  = Commands
  final override type S  = States
  final override type EE = UserRegistrationError

  override def transitions: TransitionF = ((startRegistration orElse
    (emailConfirmation << tokensMustMatch)) << identitiesMustMatch).liftK

  val startRegistration: Transition =
    case (c: StartRegistration, _: PotentialCustomer) =>
      WaitingForEmailRegistration(c.id, c.email, c.token).asRight

  val tokensMustMatch: Invariant = { case (c: ConfirmEmail, s: WaitingForEmailRegistration) =>
    if c.token.value != s.token.value then InvalidToken(c.token).invalidNec else ().validNec
  }

  val emailConfirmation: Transition =
    case (_: ConfirmEmail, s: WaitingForEmailRegistration) =>
      Active(s.id, s.email).asRight
}

object UserRegistration {

  def simple(): SimpleUserRegistration = new SimpleUserRegistration

  def withEvents(): UserRegistrationWithEvents = new UserRegistrationWithEvents

  sealed trait UserRegistrationError extends DomainError

  final case class InvalidToken(token: Token) extends UserRegistrationError {
    override def msg: String = s"Token '${token.value}' is invalid"
  }

  final case class UserId(id: UUID)     extends AnyVal
  final case class Email(value: String) extends AnyVal
  final case class Token(value: String) extends AnyVal

  enum Commands extends HasIdentity[UserId]:
    case StartRegistration(id: UserId, email: Email, token: Token)
    case ConfirmEmail(id: UserId, token: Token)
    case RestartRegistration(id: UserId)
    case DeleteDueToGDPR(id: UserId)

  enum States extends HasIdentity[UserId]:
    case PotentialCustomer(id: PreGenesis.type = PreGenesis)
    case WaitingForEmailRegistration(
        id: UserId,
        email: Email,
        token: Token,
    )
    case Active(id: UserId, email: Email)
    case Deleted(id: UserId)

  object givens {
    given isFinalState: IsEnd[States] with
      override def apply(s: States): Boolean = s match
        case _: States.Deleted => true
        case _                 => false

    given userIdEquals: EqualIdentities[UserId] with
      override def equals(idA: UserId | PreGenesis.type, idB: UserId | PreGenesis.type): Boolean =
        (idA, idB) match
          case (a: UserId, b: UserId)  => a.id.equals(b.id)
          case (_: PreGenesis.type, _) => false // Commands should always have an identity
          case (_, _: PreGenesis.type) => true  // If there is a pre-genesis state, allow
          case _                       => false
  }
}

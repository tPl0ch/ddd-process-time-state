package org.tp.process_time_state

import Lifecycle.*
import AccountRegistration.Commands.*
import AccountRegistration.States.*
import AccountRegistration.*
import AccountRegistration.givens.given
import identity.*
import impl.{ SimpleAccountRegistration, AccountRegistrationWithEvents }

import cats.data.{ Kleisli, NonEmptyChain }
import cats.instances.either.*
import cats.syntax.either.*
import cats.syntax.validated.*

import java.util.UUID

type ErrorOr[A] = Either[NonEmptyChain[AccountRegistrationError], A]

abstract class AccountRegistration extends Aggregate[ErrorOr] {

  final override type ID = AccountId
  final override type C  = Commands
  final override type S  = States
  final override type EE = AccountRegistrationError

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

object AccountRegistration {

  def simple(): SimpleAccountRegistration = new SimpleAccountRegistration

  def withEvents(): AccountRegistrationWithEvents = new AccountRegistrationWithEvents

  sealed trait AccountRegistrationError extends DomainError

  final case class InvalidToken(token: Token) extends AccountRegistrationError {
    override def msg: String = s"Token '${token.value}' is invalid"
  }

  final case class AccountId(id: UUID)  extends AnyVal
  final case class Email(value: String) extends AnyVal
  final case class Token(value: String) extends AnyVal

  enum Commands extends HasIdentity[AccountId]:
    case StartRegistration(id: AccountId, email: Email, token: Token)
    case ConfirmEmail(id: AccountId, token: Token)
    case RestartRegistration(id: AccountId)
    case DeleteDueToGDPR(id: AccountId)

  enum States extends HasIdentity[AccountId]:
    case PotentialCustomer(id: PreGenesis.type = PreGenesis)
    case WaitingForEmailRegistration(
        id: AccountId,
        email: Email,
        token: Token,
    )
    case Active(id: AccountId, email: Email)
    case Deleted(id: AccountId)

  object givens {
    given isFinalState: IsEnd[States] with
      override def apply(s: States): Boolean = s match
        case _: States.Deleted => true
        case _                 => false

    given accountIdEquals: EqualIdentities[AccountId] with
      override def equals(idA: UID[AccountId], idB: UID[AccountId]): Boolean =
        (idA, idB) match
          case (a: AccountId, b: AccountId) => a.id.equals(b.id)
          case (_: PreGenesis.type, _)      => false // Commands should always have an identity
          case (_, _: PreGenesis.type)      => true  // If there is a pre-genesis state, allow
          case _                            => false
  }
}
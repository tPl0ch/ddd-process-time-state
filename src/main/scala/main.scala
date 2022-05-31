package org.tp.process_time_state

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.validated.*
import cats.implicits.*
import Commands.*
import Events.*
import States.*
import BehaviorF.*

import java.util.UUID

final case class UserId(id: UUID)     extends AnyVal
final case class Email(value: String) extends AnyVal
final case class Token(value: String) extends AnyVal

enum Commands:
  case Register(id: UserId, email: Email)
  case Confirm(token: Token)
  case ResendConfirmation()
  case GDPRDeletion()

enum Events:
  def id: UserId

  case Registered(id: UserId, email: Email)
  case NewConfirmationRequested(id: UserId, email: Email, token: Token)
  case GDPRDeleted(id: UserId)

enum States:
  case PotentialCustomer()
  case AwaitingRegistrationConfirmation(
      userId: UserId,
      email: Email,
      token: Token,
  )
  case Active(userId: UserId, email: Email)
  case Deleted(userId: UserId)

sealed trait DomainError extends Throwable {
  def msg: String
  final override def getMessage: String = this.msg
}

final case class InvalidToken(token: Token) extends DomainError {
  override def msg: String = s"Token '${token.value}' is invalid"
}

given Conversion[DomainError, Throwable] with
  override def apply(x: DomainError): Throwable = x

type ErrorOr[A] = Either[NonEmptyList[Throwable], A]

type RegistrationBehavior = BehaviorF[ErrorOr, Commands, States]
type RegistrationGuard    = GuardF[Commands, States, DomainError]

val register: RegistrationBehavior = { case (c: Register, _: PotentialCustomer) =>
  AwaitingRegistrationConfirmation(c.id, c.email, Token("token")).asRight
}

val confirmationGuard: RegistrationGuard = {
  case (c: Confirm, s: AwaitingRegistrationConfirmation) =>
    if c.token.value != s.token.value then InvalidToken(c.token).invalidNel else ().validNel
}

val confirmationTransition: RegistrationBehavior = {
  case (_: Confirm, s: AwaitingRegistrationConfirmation) =>
    Active(s.userId, s.email).asRight
}

val confirmationAction: RegistrationBehavior = confirmationTransition << List(confirmationGuard)

/*

val register: Action = { case (c: Register, _: PotentialCustomer) =>
  AwaitingRegistrationConfirmation(c.id, c.email, Token("token"))
}

val confirmation: Action = { case (c: Confirm, s: AwaitingRegistrationConfirmation) =>
  if c.token != s.token then throw AssertionError("failed") else Active(s.userId, s.email)
}

val resend: Action = { case (_: ResendConfirmation, s: AwaitingRegistrationConfirmation) =>
  AwaitingRegistrationConfirmation(s.userId, s.email, Token("token"))
}

val delete: Action = { case (_: GDPRDeletion, s: Active) =>
  Deleted(s.userId)
}

val registeredEvent: Output = { case (_, s: AwaitingRegistrationConfirmation) =>
  NewConfirmationRequested(s.userId, s.email, s.token)
}

val confirmedEvent: Output = { case (_, s: Active) =>
  Registered(s.userId, s.email)
}

val behaviors: Action = register orElse confirmation orElse resend orElse delete

val isFinal: IsFinal[States] = _ match
  case _: Deleted => true
  case _          => false

val userRegistration = FSM.unit(behaviors, isFinal)
val userRegistrationWithEvents =
  userRegistration.toFST[Events](registeredEvent orElse confirmedEvent)

@main
def main(): Unit = {
  val userId = UserId(UUID.randomUUID())
  val email  = Email("test@example.org")
  val token  = Token("token")

  val commands = List(
    Register(userId, email),
    Confirm(token),
  )

  import FSM.*

  userRegistrationWithEvents.traverse(commands)(PotentialCustomer()).foreach(println)
}
 */

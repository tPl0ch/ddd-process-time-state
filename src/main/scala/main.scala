package org.tp.process_time_state

type Id[A] = A

type LabelF[F[_], C, S] = (C, F[S])
type Label[C, S]        = LabelF[Id, C, S]

type BehaviorsF[F[_], C, S] = PartialFunction[LabelF[F, C, S], S]
type Behaviors[C, S]        = PartialFunction[Label[C, S], S]

type IsFinalF[F[_], S] = F[S] => Boolean
type IsFinal[S]        = IsFinalF[Id, S]

type MooreF[F[_], S, E] = PartialFunction[LabelF[F, _, S], E]
type Moore[S, E]        = PartialFunction[Label[_, S], E]

type MealyF[F[_], C, S, E] = PartialFunction[LabelF[F, C, S], E]
type Mealy[C, S, E]        = PartialFunction[Label[C, S], E]

type OutputsF[F[_], C, S, E] = MooreF[F, S, E] | MealyF[F, C, S, E]
type Outputs[C, S, E]        = Moore[S, E] | Mealy[C, S, E]

opaque type MachineF[F[_], C, S] = (BehaviorsF[F, C, S], IsFinalF[F, S])
type Machine[C, S]               = (BehaviorsF[Id, C, S], IsFinalF[Id, S])

opaque type TransducerF[F[_], C, S, E] = (BehaviorsF[F, C, S], OutputsF[F, C, S, E], IsFinalF[F, S])
type Transducer[C, S, E] = (BehaviorsF[Id, C, S], OutputsF[Id, C, S, E], IsFinalF[Id, S])

extension [C, S](underlying: Machine[C, S])
  def behaviors: Behaviors[C, S] = underlying._1
  def isFinal: IsFinal[S]        = underlying._2

  def traverse(commands: Seq[C])(initialState: S): LazyList[(C, S)] =
    commands
      .foldLeft((initialState, LazyList.empty[(C, S)])) { (sa, c) =>
        val (s, acc) = sa
        if isFinal(s)
        then (s, acc)
        else {
          val newState = behaviors(c, s)
          (newState, acc.appended((c, newState)))
        }
      }
      ._2

  def toFST[E](outputs: Outputs[C, S, E]): Transducer[C, S, E] =
    val (b, f) = underlying
    (b, outputs, f)

extension [C, S, E](underlying: Transducer[C, S, E])
  def behaviors: Behaviors[C, S] = underlying._1
  def outputs: Outputs[C, S, E]  = underlying._2
  def isFinal: IsFinal[S]        = underlying._3

  def traverse(commands: Seq[C])(initialState: S): LazyList[E] =
    underlying.toFSM.traverse(commands)(initialState).map(underlying._2)

  def toFSM: Machine[C, S] =
    val (b, _, f) = underlying
    (b, f)

object FSM:

  def unit[C, S](
      behaviors: Behaviors[C, S],
      isFinal: IsFinal[S],
  ): Machine[C, S] =
    (behaviors, isFinal)

object FST:

  def unit[C, S, E](
      behaviors: Behaviors[C, S],
      outputs: Outputs[C, S, E],
      isFinal: IsFinal[S],
  ): Transducer[C, S, E] =
    (behaviors, outputs, isFinal)

import Commands.*
import Events.*
import States.*

import java.util.UUID

type Action    = Behaviors[Commands, States]
type Output    = Moore[States, Events]
type Aggregate = Machine[Commands, States]

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

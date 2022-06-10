package org.tp.process_time_state
package domain.registration

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO
import cats.implicits.*

import Identities.*
import Lifecycle.*
import Transitions.*
import Model.*
import identity.EqualId

object Behaviors {
  type EIO[A] = EitherT[IO, NonEmptyChain[AccountRegistrationError], A]
  type ID     = AccountId
  type C      = Commands
  type S      = States
  type EE     = AccountRegistrationError

  sealed trait AccountRegistrationError extends Error

  final case class InvalidToken(token: Token) extends AccountRegistrationError {
    override def msg: String = s"Token '${token.value}' is invalid"
  }

  /** This error is indicated when there is no TransitionF for a LabelIn.
    */
  final case class TransitionNotDefined(c: C, s: S) extends AccountRegistrationError {
    override def msg: String = s"Transition is not defined for command $c and state $s"
  }

  private implicit val transitionError: (C, S) => EE = (c: C, s: S) => TransitionNotDefined(c, s)

  def transitions: TransitionK[EIO, C, S, S] = ((startRegistration orElse
    (emailConfirmation << tokensMustMatch)) << identitiesMustMatch).liftF

  private val startRegistration: Transition[C, S, S, EE] = {
    case (c: Commands.StartRegistration, _: States.PotentialCustomer) =>
      States.WaitingForEmailRegistration(c.id, c.email, c.token).validNec
  }

  private val tokensMustMatch: Invariant[C, S, EE] = {
    case (c: Commands.ConfirmEmail, s: States.WaitingForEmailRegistration) =>
      if c.token.value != s.token.value then InvalidToken(c.token).invalidNec else ().validNec
  }

  private val emailConfirmation: Transition[C, S, S, EE] = {
    case (_: Commands.ConfirmEmail, s: States.WaitingForEmailRegistration) =>
      States.Active(s.id, s.email).validNec
  }

  given isFinalState: IsEnd[States] with
    override def apply(s: States): Boolean = s match
      case _: States.Deleted => true
      case _                 => false

  given accountIdEquals: EqualId[AccountId] with
    override def equals(idA: UID[AccountId], idB: UID[AccountId]): Boolean =
      (idA, idB) match
        case (a: AccountId, b: AccountId) => a.id.equals(b.id)
        case (_: NoId.type, _)            => false // Commands should always have an identity
        case (_, _: NoId.type)            => true  // If there is a pre-genesis state, allow
        case _                            => false

}

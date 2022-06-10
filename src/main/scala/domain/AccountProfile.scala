package org.tp.process_time_state
package domain

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO
import cats.implicits.*
import cats.instances.either.*
import cats.syntax.either.*

import java.util.UUID
import scala.concurrent.{ ExecutionContext, Future }

import Aggregate.*
import Lifecycle.*
import domain.AccountProfile.Commands.{ AddAddress, CreateProfile }
import domain.AccountProfile.Events.{ AddressAdded, ProfileGenerated }
import domain.AccountProfile.States.{ CompletedProfile, NoProfile, UncompletedProfile }
import domain.AccountProfile.givens.given
import domain.AccountRegistration.AccountRegistrationError
import identity.*

type ProfileEither[A] = EitherT[IO, NonEmptyChain[AccountProfile.AccountProfileError], A]

final class AccountProfile extends Aggregate[ProfileEither] with Events[ProfileEither] {

  /** The Command alphabet type as a subtype of HasIdentity[ID] */
  override type C = AccountProfile.Commands

  /** The State alphabet type as a subtype of HasIdentity[ID] */
  override type S = AccountProfile.States

  /** The Domain Error type as a subtype of DomainError */
  override type EE = AccountProfile.AccountProfileError

  /** The Event alphabet type as a subtype of HasIdentity[ID] */
  override type E = AccountProfile.Events

  /** The type of the Aggregate's identity */
  override type ID = AccountProfile.ProfileId

  /** Provides the output Events for the Aggregate
    *
    * @return
    *   The lifted partial output function that provides the Aggregate's Events within the context F
    */
  override def events: OutputsF = (profileGenerated orElse addressAdded).liftF

  /** This abstract method needs to provide all the state transitions supported by the Aggregate.
    * You can use the `mkTransitionF` helper method to easily lift the composed partial functions
    * into the Kleisli data structure.
    *
    * @see
    *   mkTransitionF
    */
  override def transitions: TransitionF =
    ((createProfile orElse addAddress) << identitiesMustMatch).liftF

  val createProfile: Transition = { case (command: CreateProfile, _: NoProfile) =>
    EitherT(IO.pure(UncompletedProfile(command.id, command.accountId).asRight))
  }

  val profileGenerated: Outputs = { case (command: CreateProfile, _: NoProfile) =>
    EitherT(IO.pure(ProfileGenerated(command.id, command.accountId).asRight))
  }

  val addAddress: Transition = { case (command: AddAddress, state: UncompletedProfile) =>
    EitherT.apply(
      IO.pure(CompletedProfile(state.id, state.accountId, command.address).asRight),
    )
  }

  val addressAdded: Outputs = { case (command: AddAddress, state: UncompletedProfile) =>
    EitherT.apply(
      IO.pure(AddressAdded(state.id, state.accountId, command.address).asRight),
    )
  }
}

object AccountProfile {
  final case class ProfileId(id: UUID) extends AnyVal
  final case class AccountId(id: UUID) extends AnyVal
  final case class Address(street: String, number: String, poBox: String)

  enum Commands extends HasIdentity[ProfileId]:
    case CreateProfile(id: ProfileId, accountId: AccountId)
    case AddAddress(id: ProfileId, address: Address)
    case DeleteProfile(id: ProfileId)

  enum States extends HasIdentity[ProfileId]:
    case NoProfile(id: Lifecycle.PreGenesis.type = PreGenesis)
    case UncompletedProfile(id: ProfileId, accountId: AccountId)
    case CompletedProfile(id: ProfileId, accountId: AccountId, address: Address)
    case DeletedProfile(id: ProfileId)

  enum Events extends HasIdentity[ProfileId]:
    case ProfileGenerated(id: ProfileId, accountId: AccountId)
    case AddressAdded(id: ProfileId, accountId: AccountId, address: Address)
    case ProfileDeleted(id: ProfileId)

  sealed trait AccountProfileError extends DomainError

  object givens {
    given isFinalState: IsEnd[States] with
      override def apply(s: States): Boolean = s match
        case _: States.DeletedProfile => true
        case _                        => false

    given accountIdEquals: EqualIdentities[ProfileId] with
      override def equals(idA: UID[ProfileId], idB: UID[ProfileId]): Boolean =
        (idA, idB) match
          case (a: ProfileId, b: ProfileId) => a.id.equals(b.id)
          case (_: PreGenesis.type, _)      => false // Commands should always have an identity
          case (_, _: PreGenesis.type)      => true  // If there is a pre-genesis state, allow
  }
}

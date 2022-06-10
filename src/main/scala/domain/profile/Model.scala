package org.tp.process_time_state
package domain.profile

import java.util.UUID

import Lifecycle.NoId
import identity.HasId

object Model {
  final case class ProfileId(id: UUID) extends AnyVal
  final case class AccountId(id: UUID) extends AnyVal
  final case class Address(street: String, number: String, poBox: String)

  enum Command extends HasId[ProfileId]:
    case CreateProfile(id: ProfileId, accountId: AccountId)
    case AddAddress(id: ProfileId, address: Address)
    case DeleteProfile(id: ProfileId)

  enum State extends HasId[ProfileId]:
    case NoProfile(id: Lifecycle.NoId.type = NoId)
    case UncompletedProfile(id: ProfileId, accountId: AccountId)
    case CompletedProfile(id: ProfileId, accountId: AccountId, address: Address)
    case DeletedProfile(id: ProfileId)

  enum Event extends HasId[ProfileId]:
    case ProfileGenerated(id: ProfileId, accountId: AccountId)
    case AddressAdded(id: ProfileId, accountId: AccountId, address: Address)
    case ProfileDeleted(id: ProfileId)
}

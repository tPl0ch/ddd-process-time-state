package org.tp.process_time_state
package domain.profile

import java.util.UUID

object Model {
  final case class ProfileId(id: UUID) extends AnyVal
  final case class AccountId(id: UUID) extends AnyVal
  final case class Address(street: String, number: String, poBox: String)

  enum Command extends Lifecycle[ProfileId]:
    case CreateProfile(id: ProfileId, accountId: AccountId)
    case AddAddress(id: ProfileId, address: Address)
    case DeleteProfile(id: ProfileId)

  enum State extends Lifecycle[ProfileId]:
    case NoProfile(id: LifecycleNotStarted.type = LifecycleNotStarted)
    case UncompletedProfile(id: ProfileId, accountId: AccountId)
    case CompletedProfile(id: ProfileId, accountId: AccountId, address: Address)
    case DeletedProfile(id: ProfileId)

  enum Event extends Lifecycle[ProfileId]:
    case ProfileGenerated(id: ProfileId, accountId: AccountId)
    case AddressAdded(id: ProfileId, accountId: AccountId, address: Address)
    case ProfileDeleted(id: ProfileId)
}

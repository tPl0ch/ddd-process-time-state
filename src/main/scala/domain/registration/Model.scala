package org.tp.process_time_state
package domain.registration

import java.util.UUID

import Lifecycle.NoId
import identity.HasId

object Model {

  final case class AccountId(id: UUID)  extends AnyVal
  final case class Email(value: String) extends AnyVal
  final case class Token(value: String) extends AnyVal

  enum Commands extends HasId[AccountId]:
    case StartRegistration(id: AccountId, email: Email, token: Token)
    case ConfirmEmail(id: AccountId, token: Token)
    case RestartRegistration(id: AccountId)
    case DeleteDueToGDPR(id: AccountId)

  enum States extends HasId[AccountId]:
    case PotentialCustomer(id: NoId.type = NoId)
    case WaitingForEmailRegistration(
        id: AccountId,
        email: Email,
        token: Token,
    )
    case Active(id: AccountId, email: Email)
    case Deleted(id: AccountId)

}

package org.tp.process_time_state
package domain.registration

import domain.registration.Model.*
import domain.registration.Types.ID

object ReadModel {
  enum Account:
    case Current(accountId: ID, email: Email, token: Option[Token])
    case Deleted(accountId: ID)
}

package org.tp.process_time_state
package domain.registration

import Model.*
import Types.ID

object ReadModel {
  enum Account:
    case Current(accountId: ID, email: Email, token: Option[Token])
    case Deleted(accountId: ID)
}

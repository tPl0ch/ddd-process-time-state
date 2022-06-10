package org.tp.process_time_state
package domain.registration

import domain.registration.Model.*

object ReadModel {
  enum Model:
    case Account(accountId: AccountId, email: Email, token: Option[Token])
    case DeletedAccount(accountId: AccountId)

  def project: Event => Model = (ev: Event) =>
    ev match
      case e: Event.RegistrationStarted => Model.Account(e.id, e.email, Some(e.token))
      case e: Event.EmailConfirmed      => Model.Account(e.id, e.email, None)
      case e: Event.GDPRDeleted         => Model.DeletedAccount(e.id)
}

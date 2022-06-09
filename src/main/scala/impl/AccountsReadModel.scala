package org.tp.process_time_state
package impl

import impl.AccountRegistration.*
import impl.AccountRegistrationWithEvents.*

import cats.Applicative
import cats.data.Kleisli
import cats.implicits.*

object AccountsReadModel {
  enum Model:
    case Account(accountId: AccountId, email: Email, token: Option[Token])
    case DeletedAccount(accountId: AccountId)

  def makeProjection[F[_]](using
      F: Applicative[F],
  ): Events => F[Model] = (ev: Events) =>
    ev match
      case e: Events.RegistrationStarted => F.pure(Model.Account(e.id, e.email, Some(e.token)))
      case e: Events.EmailConfirmed      => F.pure(Model.Account(e.id, e.email, None))
      case e: Events.GDPRDeleted         => F.pure(Model.DeletedAccount(e.id))
}

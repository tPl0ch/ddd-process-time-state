package org.tp.process_time_state
package examples

import java.util.UUID

import domain.AccountRegistration.{ AccountId, Commands, Email, Token }
import domain.{ AccountRegistration, AccountRegistrationWithEvents, SimpleAccountRegistration }

object Data {
  object AccountRegistration {
    val userId: AccountId        = AccountId(UUID.randomUUID())
    val anotherUserId: AccountId = AccountId(UUID.randomUUID())
    val email: Email             = Email("test@example.org")
    val token: Token             = Token("token")
    val anotherToken: Token      = Token("another-token")

    val commands: List[Commands] =
      List(Commands.StartRegistration(userId, email, token), Commands.ConfirmEmail(userId, token))

    val commandsWrongIdentity: List[Commands] =
      List(
        Commands.StartRegistration(userId, email, token),
        Commands.ConfirmEmail(anotherUserId, token),
      )

    val commandsWrongToken: List[Commands] =
      List(
        Commands.StartRegistration(userId, email, token),
        Commands.ConfirmEmail(userId, anotherToken),
      )

    val commandsNoTransition: List[Commands] =
      List(Commands.StartRegistration(userId, email, token), Commands.DeleteDueToGDPR(userId))

    val withoutEvents: SimpleAccountRegistration  = AccountRegistration.withoutEvents
    val withEvents: AccountRegistrationWithEvents = AccountRegistration.withEvents
  }
}
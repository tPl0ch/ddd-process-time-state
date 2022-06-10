package org.tp.process_time_state
package examples

import java.util.UUID

import domain.registration.Model.{ AccountId, Command, Email, Token }

object Data {
  object AccountRegistration {
    val userId: AccountId        = AccountId(UUID.randomUUID())
    val anotherUserId: AccountId = AccountId(UUID.randomUUID())
    val email: Email             = Email("test@example.org")
    val token: Token             = Token("token")
    val anotherToken: Token      = Token("another-token")

    val commands: List[Command] =
      List(Command.StartRegistration(userId, email, token), Command.ConfirmEmail(userId, token))

    val commandsWrongIdentity: List[Command] =
      List(
        Command.StartRegistration(userId, email, token),
        Command.ConfirmEmail(anotherUserId, token),
      )

    val commandsWrongToken: List[Command] =
      List(
        Command.StartRegistration(userId, email, token),
        Command.ConfirmEmail(userId, anotherToken),
      )

    val commandsNoTransition: List[Command] =
      List(Command.StartRegistration(userId, email, token), Command.DeleteDueToGDPR(userId))
  }
}

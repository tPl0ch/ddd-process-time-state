package org.tp.process_time_state
package examples

import java.util.UUID

import domain.registration.Model.{ AccountId, Command, Email, Token, Event }

object Data {
  object Registration {
    val userId: AccountId        = AccountId(UUID.randomUUID())
    val anotherUserId: AccountId = AccountId(UUID.randomUUID())
    val email: Email             = Email("test@example.org")
    val token: Token             = Token("token")
    val anotherToken: Token      = Token("another-token")

    val startRegistration: Command = Command.StartRegistration(userId, email, token)
    val registrationStarted: Event = Event.RegistrationStarted(userId, email, token)
    val confirmEmail: Command      = Command.ConfirmEmail(userId, token)

    val commands: List[Command] =
      List(startRegistration, confirmEmail)

    val commandsWrongIdentity: List[Command] =
      List(
        startRegistration,
        Command.ConfirmEmail(anotherUserId, token),
      )

    val commandsWrongToken: List[Command] =
      List(
        startRegistration,
        Command.ConfirmEmail(userId, anotherToken),
      )

    val commandsNoTransition: List[Command] =
      List(startRegistration, Command.DeleteDueToGDPR(userId))
  }
}

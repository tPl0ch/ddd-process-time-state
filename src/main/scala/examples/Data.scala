package org.tp.process_time_state
package examples

import java.util.UUID

import domain.registration.Behaviors.*
import domain.registration.Events.*
import domain.registration.Model.*
import domain.registration.Types.*

object Data {
  object Registration {
    val accountId: ID        = AccountId(UUID.randomUUID())
    val anotherAccountId: ID = AccountId(UUID.randomUUID())
    val email: Email         = Email("test@example.org")
    val token: Token         = Token("token")
    val anotherToken: Token  = Token("another-token")

    val startRegistration: C   = Command.StartRegistration(accountId, email, token)
    val registrationStarted: E = Event.RegistrationStarted(accountId, email, token)
    val confirmEmail: C        = Command.ConfirmEmail(accountId, token)

    val commands: List[C] =
      List(startRegistration, confirmEmail)

    val commandsWrongIdentity: List[C] =
      List(
        startRegistration,
        Command.ConfirmEmail(anotherAccountId, token),
      )

    val commandsWrongToken: List[C] =
      List(
        startRegistration,
        Command.ConfirmEmail(accountId, anotherToken),
      )

    val commandsNoTransition: List[C] =
      List(startRegistration, Command.DeleteDueToGDPR(accountId))
  }
}

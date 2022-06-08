package org.tp.process_time_state

import AccountRegistration.*
import FST.*
import Aggregate.*

import cats.implicits.*

import java.util.UUID

@main
def main(): Unit = {
  val userId        = AccountId(UUID.randomUUID())
  val anotherUserId = AccountId(UUID.randomUUID())
  val email         = Email("test@example.org")
  val token         = Token("token")
  val anotherToken  = Token("another-token")

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

  val simple = AccountRegistration.simple()
  List(
    simple,
    simple.toFSM.run(commands)(States.PotentialCustomer()),
    simple.toFSM.run(commandsWrongIdentity)(States.PotentialCustomer()),
    simple.toFSM.run(commandsWrongToken)(States.PotentialCustomer()),
    simple.toFSM.run(commandsNoTransition)(States.PotentialCustomer()),
  ).foreach(println)

  val withEvents = AccountRegistration.withEvents()
  List(
    s"$withEvents FSM",
    withEvents.toFSM.run(commands)(States.PotentialCustomer()),
    withEvents.toFSM.run(commandsWrongIdentity)(States.PotentialCustomer()),
    withEvents.toFSM.run(commandsWrongToken)(States.PotentialCustomer()),
    withEvents.toFSM.run(commandsNoTransition)(States.PotentialCustomer()),
    s"$withEvents FST",
    withEvents.toFST.run(commands)(States.PotentialCustomer()),
    withEvents.toFST.run(commandsWrongIdentity)(States.PotentialCustomer()),
    withEvents.toFST.run(commandsWrongToken)(States.PotentialCustomer()),
    withEvents.toFST.run(commandsNoTransition)(States.PotentialCustomer()),
  ).foreach(println)
}

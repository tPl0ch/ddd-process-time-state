package org.tp.process_time_state

import UserRegistration.*

import java.util.UUID

@main
def main(): Unit = {
  val userId        = UserId(UUID.randomUUID())
  val anotherUserId = UserId(UUID.randomUUID())
  val email         = Email("test@example.org")
  val token         = Token("token")
  val anotherToken  = Token("another-token")

  val aggregateImplementations = List(UserRegistration.simple(), UserRegistration.withEvents())

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

  aggregateImplementations.foreach(aggregate =>
    List(
      aggregate,
      aggregate.traverse(commands)(States.PotentialCustomer()),
      aggregate.traverse(commandsWrongIdentity)(States.PotentialCustomer()),
      aggregate.traverse(commandsWrongToken)(States.PotentialCustomer()),
      aggregate.traverse(commandsNoTransition)(States.PotentialCustomer()),
    ).foreach(println),
  )
}

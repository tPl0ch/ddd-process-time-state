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

  val userRegistration = UserRegistration()
  val commands: List[Commands] =
    List(Commands.Register(userId, email, token), Commands.Confirm(userId, token))

  val commandsWrongIdentity: List[Commands] =
    List(Commands.Register(userId, email, token), Commands.Confirm(anotherUserId, token))

  val commandsWrongToken: List[Commands] =
    List(Commands.Register(userId, email, token), Commands.Confirm(userId, anotherToken))

  println(
    userRegistration.traverse(commands)(States.PotentialCustomer()),
  )
  println(
    userRegistration.traverse(commandsWrongIdentity)(States.PotentialCustomer()),
  )
  println(
    userRegistration.traverse(commandsWrongToken)(States.PotentialCustomer()),
  )
}

package org.tp
package process_time_state

import UserRegistration.*

import java.util.UUID

@main
def main(): Unit = {
  val userId = UserId(UUID.randomUUID())
  val email  = Email("test@example.org")
  val token  = Token("token")
}

package org.tp.process_time_state
package domain.registration

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO

import domain.registration.Errors.AccountRegistrationError
import domain.registration.Model.{ AccountId, Command, Event, State }

object Types {
  type EIO[A] = EitherT[IO, NonEmptyChain[AccountRegistrationError], A]
  type ID     = AccountId
  type C      = Command
  type S      = State
  type E      = Event
  type EE     = AccountRegistrationError

  type StateTransition = Transition[C, S, S, EE]
  type EventOutput     = Transition[C, S, E, EE]
}

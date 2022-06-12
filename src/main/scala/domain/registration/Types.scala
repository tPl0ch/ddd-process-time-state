package org.tp.process_time_state
package domain.registration

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO

import domain.registration.Errors.RegistrationError
import domain.registration.Model.{ AccountId, Command, Event, State }

object Types {
  type EIO[A] = EitherT[IO, NonEmptyChain[RegistrationError], A]
  type ID     = AccountId
  type C      = Command
  type S      = State
  type E      = Event
  type R      = ReadModel.Model
  type EE     = RegistrationError

  type StateTransition = Behavior[C, S, EE]
  type EventOutput     = Output[C, S, E]

  type StateMachine = FSM[EIO, C, S]
  type Transducer   = FST[EIO, C, S, E]
  type Projection   = FST[EIO, C, S, R]
}

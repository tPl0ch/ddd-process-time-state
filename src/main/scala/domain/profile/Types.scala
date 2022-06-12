package org.tp.process_time_state
package domain.profile

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO

import domain.profile.Errors.ProfileError
import domain.profile.Model.*

object Types {
  type EIO[A] = EitherT[IO, NonEmptyChain[ProfileError], A]
  type ID     = ProfileId
  type C      = Command
  type S      = State
  type E      = Event
  type EE     = ProfileError

  type StateTransition = Behavior[C, S, EE]
  type EventOutput     = Output[C, S, E]

  type StateMachine = FSM[EIO, C, S]
  type Transducer   = FST[EIO, C, S, E]
}

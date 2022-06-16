package org.tp.process_time_state
package domain.profile

import cats.implicits.*
import cats.syntax.validated.*

import Givens.given
import Model.*
import Types.*

object Behaviors {

  def behaviors: BehaviorsK[EIO, C, S] =
    (profileGeneration orElse addressDefinition).liftLifecycleF

  val profileGeneration: StateTransition = {
    case (command: Command.CreateProfile, _: State.NoProfile) =>
      State.UncompletedProfile(command.id, command.accountId).validNec
  }

  val addressDefinition: StateTransition = {
    case (command: Command.AddAddress, state: State.UncompletedProfile) =>
      State.CompletedProfile(state.id, state.accountId, command.address).validNec
  }
}

package org.tp.process_time_state
package domain.profile

import cats.implicits.*
import cats.syntax.validated.*

import Transitions.*
import domain.profile.Errors.ProfileError
import domain.profile.Givens.given
import domain.profile.Model.*
import domain.profile.Types.*

object Behaviors {

  def behaviors: BehaviorsK[EIO, C, S] =
    (profileGeneration orElse addressDefinition).liftF

  val profileGeneration: StateTransition = {
    case (command: Command.CreateProfile, _: State.NoProfile) =>
      State.UncompletedProfile(command.id, command.accountId).validNec
  }

  val addressDefinition: StateTransition = {
    case (command: Command.AddAddress, state: State.UncompletedProfile) =>
      State.CompletedProfile(state.id, state.accountId, command.address).validNec
  }
}

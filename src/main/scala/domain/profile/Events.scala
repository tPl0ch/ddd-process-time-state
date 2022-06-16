package org.tp.process_time_state
package domain.profile

import cats.implicits.*

import domain.profile.Errors.ProfileError
import domain.profile.Givens.given
import domain.profile.Model.*
import domain.profile.Types.*

object Events {

  def events: OutputsK[EIO, C, S, E] =
    (profileGenerated orElse addressAdded).liftF

  val profileGenerated: EventOutput = { case (command: Command.CreateProfile, _: State.NoProfile) =>
    Event.ProfileGenerated(command.id, command.accountId)
  }

  val addressAdded: EventOutput = {
    case (command: Command.AddAddress, state: State.UncompletedProfile) =>
      Event.AddressAdded(state.id, state.accountId, command.address)
  }
}

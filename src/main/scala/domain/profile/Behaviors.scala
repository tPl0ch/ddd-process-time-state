package org.tp.process_time_state
package domain.profile

import cats.implicits.*
import cats.syntax.validated.*

import Transitions.*
import domain.profile.Errors.{ ProfileError, TransitionNotDefined }
import domain.profile.Givens.given
import domain.profile.Model.*
import domain.profile.Types.*

object Behaviors {

  def transitions: TransitionK[EIO, C, S, S] =
    (createProfile orElse addAddress).liftF

  val createProfile: StateTransition = {
    case (command: Command.CreateProfile, _: State.NoProfile) =>
      State.UncompletedProfile(command.id, command.accountId).validNec
  }

  val addAddress: StateTransition = {
    case (command: Command.AddAddress, state: State.UncompletedProfile) =>
      State.CompletedProfile(state.id, state.accountId, command.address).validNec
  }

  private implicit val transitionError: (C, S) => EE = (c: C, s: S) => TransitionNotDefined(c, s)
}

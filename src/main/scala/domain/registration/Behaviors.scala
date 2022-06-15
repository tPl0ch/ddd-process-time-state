package org.tp.process_time_state
package domain.registration

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO
import cats.implicits.*

import Identities.identitiesMustMatch
import Transitions.*
import domain.registration.Errors.*
import domain.registration.Givens.given
import domain.registration.Model.*
import domain.registration.Types.*

object Behaviors {

  def behaviors: BehaviorsK[EIO, C, S] = ((registration orElse
    (emailConfirmation << tokensMustMatch)) << identitiesMustMatch).liftLifecycleF

  val registration: Behavior[C, S, EE] = {
    case (c: Command.StartRegistration, _: State.PotentialCustomer) =>
      State.WaitingForEmailRegistration(c.id, c.email, c.token).validNec
  }

  val tokensMustMatch: Invariant[C, S, EE] = {
    case (c: Command.ConfirmEmail, s: State.WaitingForEmailRegistration) =>
      if c.token.value != s.token.value then InvalidToken(c.token).invalidNec else ().validNec
  }

  val emailConfirmation: Behavior[C, S, EE] = {
    case (_: Command.ConfirmEmail, s: State.WaitingForEmailRegistration) =>
      State.Active(s.id, s.email).validNec
  }
}

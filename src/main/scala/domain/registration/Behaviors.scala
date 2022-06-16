package org.tp.process_time_state
package domain.registration

import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.IO
import cats.implicits.*

import domain.registration.Errors.*
import domain.registration.Givens.given
import domain.registration.Model.*
import domain.registration.Types.*

object Behaviors {

  def behaviors: BehaviorsK[EIO, C, S] = ((registration orElse
    (emailConfirmation << tokensMustMatch)) << identitiesMustMatch).liftLifecycleF

  val registration: RegistrationBehavior = {
    case (c: Command.StartRegistration, _: State.PotentialCustomer) =>
      State.WaitingForEmailRegistration(c.id, c.email, c.token).validNec
  }

  val tokensMustMatch: RegistrationInvariant = {
    case (c: Command.ConfirmEmail, s: State.WaitingForEmailRegistration) =>
      if c.token.value != s.token.value then InvalidToken(c.token).invalidNec else ().validNec
  }

  val emailConfirmation: RegistrationBehavior = {
    case (_: Command.ConfirmEmail, s: State.WaitingForEmailRegistration) =>
      State.Active(s.id, s.email).validNec
  }
}

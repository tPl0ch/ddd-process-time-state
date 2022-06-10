package org.tp.process_time_state
package examples

import cats.Applicative

import domain.registration.Model.{ Event, State }

trait RegistrationRepositories[F[_]] {
  def loadState[F[_]](using F: Applicative[F]): F[State] =
    F.pure(State.PotentialCustomer())

  def saveState[F[_]](using F: Applicative[F]): State => F[Unit] = _ => F.unit

  def saveEvent[F[_]](using F: Applicative[F]): Event => F[Unit] = _ => F.unit
}

package org.tp.process_time_state
package examples

import cats.Applicative

import domain.registration.Model.{ AccountId, Event, State }
import domain.registration.ReadModel.Account
import domain.registration.Types.*

trait RegistrationRepositories[F[_]] {
  def loadState[F[_]](using AP: Applicative[F]): Identity[ID] => F[S] =
    _ => AP.pure(State.PotentialCustomer())

  def saveState[F[_]](using AP: Applicative[F]): S => F[Unit] = _ => AP.unit

  def loadEventStream[F[_]](using AP: Applicative[F]): Identity[ID] => F[Seq[E]] =
    _ => AP.pure(List(Data.Registration.registrationStartedEvent))
  def saveEvent[F[_]](using AP: Applicative[F]): E => F[Unit]           = _ => AP.unit
  def saveEvents[F[_]](using AP: Applicative[F]): List[E] => F[Unit]    = _ => AP.unit
  def transactionalOutbox[F[_]](using AP: Applicative[F]): E => F[Unit] = _ => AP.unit

  def saveReadModel[F[_]](using AP: Applicative[F]): Account => F[Unit] = _ => AP.unit
}

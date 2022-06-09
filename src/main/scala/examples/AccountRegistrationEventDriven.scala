package org.tp.process_time_state
package examples

import Aggregate.*

import Data.*
import FST.*
import domain.AccountRegistration.*
import domain.{
  AccountRegistration,
  AccountRegistrationWithEvents,
  AccountsReadModel,
  RegistrationEither,
}

import cats.Applicative
import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.syntax.either.*

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

object AccountRegistrationEventDriven extends IOApp.Simple {

  type EV = domain.AccountRegistrationWithEvents.Events

  def loadState[F[_]](using F: Applicative[F]): F[States] =
    F.pure(States.PotentialCustomer())

  def storeEvent[F[_]](using
      F: Applicative[F],
  ): EV => F[Unit] = _ => F.unit

  val accountRegistration: FST[RegistrationEither, Commands, States, EV] =
    (command: Commands) =>
      for {
        (currentState, event) <- withEvents.toFST(command)
        _                     <- StateK.lift(storeEvent[RegistrationEither](event))
      } yield (currentState, event)

  val accountRegistrationIO: EitherT[IO, NonEmptyChain[AccountRegistrationError], Unit] =
    for {
      initialState <- loadState[RegistrationEither]
      stateAndEvents <- accountRegistration.traverse(Data.AccountRegistration.commands)(
        initialState,
      )
      _ <- EitherT(IO(println(stateAndEvents).asRight))
    } yield ()

  override def run: IO[Unit] = accountRegistrationIO.value.map(_ => ())
}

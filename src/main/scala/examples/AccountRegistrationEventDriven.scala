package org.tp.process_time_state
package examples

import cats.Applicative
import cats.data.{ EitherT, NonEmptyChain, StateT }
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.syntax.either.*

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

import Aggregate.*
import FST.*
import domain.AccountRegistration.*
import domain.{
  AccountRegistration,
  AccountRegistrationWithEvents,
  AccountsReadModel,
  RegistrationEither,
}
import examples.Data.*

object AccountRegistrationEventDriven extends IOApp.Simple {

  type EV = domain.AccountRegistrationWithEvents.Events

  def loadState[F[_]](using F: Applicative[F]): F[States] =
    F.pure(States.PotentialCustomer())

  def storeEvent[F[_]](using
      F: Applicative[F],
  ): EV => F[Unit] = _ => F.unit

  private val accountRegistration =
    (command: Commands) =>
      for {
        event <- withEvents.toFST(command)
        _     <- StateT.liftF(storeEvent[RegistrationEither](event))
      } yield event

  private val accountRegistrationIO =
    for {
      initialState <- loadState[RegistrationEither]
      stateAndEvents <- accountRegistration
        .traverse(Data.AccountRegistration.commands)
        .run(
          initialState,
        )
    } yield stateAndEvents

  override def run: IO[Unit] =
    accountRegistrationIO.value.flatTap(se => IO(println(se))).map(_ => ())
}

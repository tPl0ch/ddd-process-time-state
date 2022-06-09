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

object ReadModelProjection extends IOApp.Simple {

  def loadInitialState[F[_]](using F: Applicative[F]): F[States] =
    F.pure(States.PotentialCustomer())

  def storeReadModel[F[_]](using
      F: Applicative[F],
  ): AccountsReadModel.Model => F[Unit] = _ => F.pure(())

  def storeEvent[F[_]](using
      F: Applicative[F],
  ): AccountRegistrationWithEvents.Events => F[Unit] = _ => F.pure(())

  val projection: FST[
    RegistrationEither,
    Commands,
    States,
    (AccountRegistrationWithEvents.Events, AccountsReadModel.Model),
  ] =
    (c: Commands) =>
      for {
        (s, event)     <- withEvents.toFST(c)
        _              <- StateK.lift(storeEvent[RegistrationEither](event))
        (_, readModel) <- StateK.lift(AccountsReadModel.makeProjection[RegistrationEither](event))
        _              <- StateK.lift(storeReadModel[RegistrationEither](readModel))
      } yield (s, (event, readModel))

  val projectionIO: EitherT[IO, NonEmptyChain[AccountRegistrationError], Unit] = for {
    initialState    <- loadInitialState[RegistrationEither]
    (_, readModels) <- projection.traverse(Data.AccountRegistration.commands)(initialState)
    _               <- EitherT(IO(println(readModels).asRight))
  } yield ()

  override def run: IO[Unit] = projectionIO.value.map(_ => ())
}

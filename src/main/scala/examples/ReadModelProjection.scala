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

object ReadModelProjection extends IOApp.Simple {

  def loadInitialState[F[_]](using F: Applicative[F]): F[States] =
    F.pure(States.PotentialCustomer())

  def storeReadModel[F[_]](using
      F: Applicative[F],
  ): AccountsReadModel.Model => F[Unit] = _ => F.pure(())

  def storeEvent[F[_]](using
      F: Applicative[F],
  ): AccountRegistrationWithEvents.Events => F[Unit] = _ => F.pure(())

  private val projection =
    (c: Commands) =>
      for {
        event     <- withEvents.toFST(c)
        _         <- StateT.liftF(storeEvent[RegistrationEither](event))
        readModel <- StateT.liftF(AccountsReadModel.makeProjection[RegistrationEither](event))
        _         <- StateT.liftF(storeReadModel[RegistrationEither](readModel))
      } yield (event, readModel)

  private val projectionIO = for {
    initialState    <- loadInitialState[RegistrationEither]
    (_, readModels) <- projection.traverse(Data.AccountRegistration.commands).run(initialState)
    _               <- EitherT(IO(println(readModels).asRight))
  } yield ()

  override def run: IO[Unit] = projectionIO.value.map(_ => ())
}

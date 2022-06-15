package org.tp.process_time_state
package examples

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

import cats.data.{ EitherT, StateT }
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.syntax.either.*

import Aggregates.*
import domain.registration.Types.*
import domain.registration.ReadModel.*
import examples.Data.*

object ReadModelProjection extends IOApp.Simple with RegistrationRepositories[EIO] {

  val projection: Projection = (c: C) =>
    for {
      readModel <- AccountRegistration.aggregate(c).map(project)
      _         <- StateT.liftF(saveReadModel[EIO](readModel))
    } yield readModel

  val projectionIO: EIO[List[R]] = for {
    initialState <- loadState[EIO]
    readModels   <- projection.runAllEvents(Data.Registration.commands)(initialState)
    _            <- EitherT(IO(println(readModels).asRight))
  } yield readModels

  override def run: IO[Unit] = projectionIO.value.as(())
}

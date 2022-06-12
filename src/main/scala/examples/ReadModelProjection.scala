package org.tp.process_time_state
package examples

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

import cats.data.{ EitherT, StateT }
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.syntax.either.*

import Machines.*
import domain.registration.Imports.{ *, given }
import examples.Data.*

object ReadModelProjection extends IOApp.Simple with RegistrationRepositories[EIO] {

  val projection: Projection = (c: Command) =>
    for {
      readModel <- AccountRegistration.registration(c).map(project)
      _         <- StateT.liftF(saveReadModel[EIO](readModel))
    } yield readModel

  val projectionIO: EIO[List[R]] = for {
    initialState <- loadState[EIO]
    (_, readModels) <- projection
      .traverse(Data.Registration.commands)
      .run(initialState)
    _ <- EitherT(IO(println(readModels).asRight))
  } yield readModels

  override def run: IO[Unit] = projectionIO.value.as(())
}

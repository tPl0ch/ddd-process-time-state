package org.tp.process_time_state
package examples

import cats.data.StateT
import cats.effect.{ IO, IOApp }
import cats.implicits.*

import Data.*
import domain.registration.Machines.*
import domain.registration.Model.Event.*
import domain.registration.ReadModel.Account.*
import domain.registration.Types.*

object ReadModelProjection extends IOApp.Simple with RegistrationRepositories[EIO] {

  def project: E => R = (ev: E) =>
    ev match
      case e: RegistrationStarted => Current(e.id, e.email, Some(e.token))
      case e: EmailConfirmed      => Current(e.id, e.email, None)
      case e: GDPRDeleted         => Deleted(e.id)

  val projection: Projection = (c: C) =>
    for {
      readModel <- EventSourcedRegistration
        .eventStoringAggregate(c)
        .map(project)
      _ <- StateT.liftF(saveReadModel[EIO](readModel))
    } yield readModel

  val projectionIO: (Identity[ID], Seq[C]) => EIO[Seq[R]] = (uid, commands) =>
    for {
      initialState <- loadState[EIO](uid)
      readModels   <- projection.runAllEvents(commands)(initialState)
    } yield readModels

  override def run: IO[Unit] = projectionIO(
    LifecycleNotStarted,
    Data.Registration.commands,
  ).value.flatMap(e => IO.println(e.map(_.toList)))
}

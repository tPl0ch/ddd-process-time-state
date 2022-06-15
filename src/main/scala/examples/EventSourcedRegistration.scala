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
import EventSourcing.*
import domain.registration.Model.*
import domain.registration.Types.*
import examples.Data.*

object EventSourcedRegistration extends IOApp.Simple with RegistrationRepositories[EIO] {

  val reconstitution: (E, S) => S = { case (e, s) =>
    (e, s) match
      case (rs: Event.RegistrationStarted, _: State.PotentialCustomer) =>
        State.WaitingForEmailRegistration(rs.id, rs.email, rs.token)
  }

  val eventSourcingIO: EIO[E] = for {
    snapshot       <- loadState[EIO]
    previousEvents <- loadEventStream[EIO]
    sourcedState   <- reconstituteState[EIO, S, E](reconstitution)(snapshot)(previousEvents)
    newEvent <- AccountRegistration
      .aggregate(Data.Registration.confirmEmail)
      .runA(sourcedState)
    _ <- saveEvent[EIO](newEvent)
    _ <- EitherT(IO(println(newEvent).asRight))
  } yield newEvent

  override def run: IO[Unit] = eventSourcingIO.value.as(())
}

package org.tp.process_time_state
package examples

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

import cats.Applicative
import cats.data.{ EitherT, NonEmptyChain, StateT }
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.syntax.either.*

import Machines.*
import domain.registration.Behaviors.*
import domain.registration.Events.*
import domain.registration.Givens.given
import domain.registration.Model.*
import domain.registration.Types.*
import examples.Data.*

object RegistrationEventDriven extends IOApp.Simple with RegistrationRepositories[EIO] {

  val accountRegistration: Transducer = (command: Command) =>
    for {
      event <- Machines(transitions)(events)(command)
      _     <- StateT.liftF(saveEvent[EIO](event))
    } yield event

  val accountRegistrationIO: EIO[List[Event]] = for {
    initialState <- loadState[EIO]
    (_, listOfEvents) <- accountRegistration
      .traverse(Data.AccountRegistration.commands)
      .run(initialState)
    _ <- EitherT.liftF(IO(println(listOfEvents)))
  } yield listOfEvents

  override def run: IO[Unit] = accountRegistrationIO.value.as(())
}

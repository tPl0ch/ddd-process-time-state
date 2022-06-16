package org.tp.process_time_state
package examples

import cats.data.StateT
import cats.effect.{ IO, IOApp }
import cats.implicits.*

import Lifecycle.NotStarted
import domain.registration.Machines.*
import domain.registration.Types.*
import examples.Data.*

object StateStoreAndTransactionalOutbox extends IOApp.Simple with RegistrationRepositories[EIO] {

  val eventPublishingRegistration: Transducer = command =>
    val stateT = registrationTransducer(command)
    for {
      event <- stateT
      state <- stateT.get
      _ <- StateT.liftF(for {
        _ <- saveState[EIO](state)
        _ <- transactionalOutbox[EIO](event)
      } yield ())
    } yield event

  val programEIO: (UID[ID], Seq[C]) => EIO[Seq[E]] = (uid, commands) =>
    for {
      initialState <- loadState[EIO](uid)
      listOfEvents <- registrationTransducer.runAllEvents(commands)(initialState)
    } yield listOfEvents

  override def run: IO[Unit] = programEIO(
    NotStarted,
    Registration.commands,
  ).value.flatMap(e => IO.println(e.map(_.toList)))
}

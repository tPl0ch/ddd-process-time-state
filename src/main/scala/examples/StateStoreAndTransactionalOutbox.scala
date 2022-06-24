package org.tp.process_time_state
package examples

import cats.data.StateT
import cats.effect.{ IO, IOApp }
import cats.implicits.*

import domain.registration.Machines.*
import domain.registration.Types.*
import examples.Data.*

object StateStoreAndTransactionalOutbox extends IOApp.Simple with RegistrationRepositories[EIO] {

  val storeAndPublish: (s: S, e: E) => StateT[EIO, S, Unit] = (s, e) =>
    StateT.liftF(for {
      _ <- saveState[EIO](s)
      _ <- transactionalOutbox[EIO](e)
    } yield ())

  val eventPublishingRegistration: Transducer = command =>
    val stateT = registrationTransducer(command)
    for {
      event <- stateT
      state <- stateT.get
      _     <- storeAndPublish(state, event)
    } yield event

  val programEIO: (Identity[ID], Seq[C]) => EIO[Seq[E]] = (uid, commands) =>
    for {
      initialState <- loadState[EIO](uid)
      listOfEvents <- registrationTransducer.runAllEvents(commands)(initialState)
    } yield listOfEvents

  override def run: IO[Unit] = programEIO(
    LifecycleNotStarted,
    Registration.commands,
  ).value.flatMap(e => IO.println(e.map(_.toList)))
}

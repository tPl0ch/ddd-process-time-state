package org.tp.process_time_state
package examples

import cats.data.StateT
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.syntax.validated.*

import EventSourcing.*
import domain.registration.Machines.*
import domain.registration.Model.*
import domain.registration.Types.*
import examples.Data.*

object EventSourcedRegistration extends IOApp.Simple with RegistrationRepositories[EIO] {

  val reconstitution: (E, S) => InvariantError[EE, S] = { case (e, s) =>
    (e, s) match
      case (rs: Event.RegistrationStarted, _: State.PotentialCustomer) =>
        State.WaitingForEmailRegistration(rs.id, rs.email, rs.token).validNec
      case _ => CannotReconstituteFrom(e, s).asInstanceOf[EE].invalidNec
  }

  val eventStoringAggregate: Transducer = command =>
    for {
      newEvent <- registrationTransducer(command)
      _        <- StateT.liftF(saveEvent[EIO](newEvent))
    } yield newEvent

  val eventSourcingIO: (Identity[ID], Seq[C]) => EIO[Seq[E]] = (uid, commands) =>
    for {
      snapshot     <- loadState[EIO](uid)
      events       <- loadEventStream[EIO](uid)
      sourcedState <- reconstituteState[EIO, S, E, EE](reconstitution)(snapshot)(events)
      newEvents    <- eventStoringAggregate.runAllEvents(commands)(sourcedState)
    } yield newEvents

  override def run: IO[Unit] =
    eventSourcingIO(
      Registration.accountId,
      Seq(Registration.confirmEmail),
    ).value.flatMap(e => IO.println(e.map(_.toList)))
}

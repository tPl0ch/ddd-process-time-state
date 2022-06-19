package org.tp.process_time_state
package examples

import cats.data.StateT
import cats.effect.{ IO, IOApp }
import cats.implicits.*

import domain.registration.Givens.given
import domain.registration.Machines.*
import domain.registration.Types.*
import examples.Data.*

/** This example shows how to use a simple state machine to produce and store a state within an IO.
  */
object RegistrationStateOnly extends IOApp.Simple with RegistrationRepositories[EIO] {

  val stateStoringRegistration: StateMachine = command =>
    for {
      newState <- registrationStateMachine(command).get
      _        <- StateT.liftF(saveState[EIO](newState))
    } yield ()

  val accountRegistrationIO: (Identity[ID], Seq[C]) => EIO[S] = (uid, commands) =>
    for {
      initialState <- loadState[EIO](uid)
      currentState <- stateStoringRegistration.runAllState(commands)(
        initialState,
      )
    } yield currentState

  override def run: IO[Unit] =
    accountRegistrationIO(
      LifecycleNotStarted,
      Registration.commands,
    ).value.flatMap(IO.println(_))
}

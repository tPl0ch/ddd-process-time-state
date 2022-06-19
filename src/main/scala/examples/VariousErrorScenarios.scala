package org.tp.process_time_state
package examples

import cats.data.StateT
import cats.effect.{ IO, IOApp }
import cats.implicits.*

import domain.registration.Behaviors.behaviors
import domain.registration.Givens.given
import domain.registration.Machines.*
import domain.registration.Model.*
import domain.registration.Types.*
import examples.Data.*

/** This example shows how to use a simple state machine to produce and store a state within an IO.
  */
object VariousErrorScenarios extends IOApp.Simple with RegistrationRepositories[EIO] {

  def runErrorSequence(commands: Seq[C]): IO[Unit] = RegistrationStateOnly
    .accountRegistrationIO(
      LifecycleNotStarted,
      commands,
    )
    .value
    .flatMap(e => IO.println(e))

  val acceptingErrors: IO[Unit] =
    runErrorSequence(Registration.commandsWrongToken)
      *> runErrorSequence(Registration.commandsWrongIdentity)
      *> runErrorSequence(Registration.commandsNoTransition)

  val outputMissing: IO[Unit] =
    Aggregate
      .apply[EIO, C, S, E](behaviors)(Registration.missingEventOutput)
      .runAllEvents(Registration.commands)(State.PotentialCustomer())
      .value
      .flatMap(e => IO.println(e))

  override def run: IO[Unit] = acceptingErrors *> outputMissing
}

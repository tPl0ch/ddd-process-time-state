package org.tp.process_time_state
package examples

import cats.Applicative
import cats.data.{ EitherT, NonEmptyChain, StateT }
import cats.effect.implicits.*
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.instances.either.*
import cats.syntax.either.*

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

import Data.*
import Machines.*
import domain.registration.Behaviors.*
import domain.registration.Givens.given
import domain.registration.Model.*
import domain.registration.Types.*

/** This example shows how to use a simple state machine to produce and store a state within an IO.
  */
object RegistrationStateOnly extends IOApp.Simple with RegistrationRepositories[EIO] {

  val accountRegistration: StateMachine = (command: Command) =>
    for {
      currentState <- Machines(transitions)(command).get
      _            <- StateT.liftF(saveState[EIO](currentState))
    } yield ()

  val accountRegistrationIO: EIO[Unit] =
    for {
      initialState <- loadState[EIO]
      (currentState, _) <- accountRegistration
        .traverse(AccountRegistration.commands)
        .run(initialState)
      _ <- EitherT(IO(println(currentState).asRight))
    } yield ()

  override def run: IO[Unit] = accountRegistrationIO.value.as(())
}

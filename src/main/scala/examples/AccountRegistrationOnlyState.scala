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

object AccountRegistrationOnlyState extends IOApp.Simple {

  def loadState[F[_]](using F: Applicative[F]): F[State] =
    F.pure(State.PotentialCustomer())

  def saveState[F[_]](using F: Applicative[F]): State => F[Unit] = _ => F.unit

  val accountRegistration: FSM[EIO, Command, State] = (command: Command) =>
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

  override def run: IO[Unit] = accountRegistrationIO.value.map(_ => ())
}

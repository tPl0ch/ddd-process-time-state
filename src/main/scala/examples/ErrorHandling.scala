package org.tp.process_time_state
package examples

import cats.data.{ EitherT, NonEmptyChain, StateT }
import cats.effect.implicits.*
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.instances.either.*
import cats.syntax.either.*
import cats.{ Applicative, ApplicativeError }

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

import Aggregate.*
import FST.*
import domain.AccountRegistration.*
import domain.AccountRegistration.givens.given
import domain.{
  AccountRegistration,
  AccountRegistrationWithEvents,
  AccountsReadModel,
  RegistrationEither,
}

object ErrorHandling extends IOApp.Simple {

  def loadState[F[_]](using F: Applicative[F]): F[States] =
    F.pure(States.PotentialCustomer())

  def saveState[F[_]](using F: Applicative[F]): States => F[Unit] = _ => F.unit

  case object SavingFailed extends DomainError {
    override def msg: String = "Failed to save state!"
  }

  def saveStateFailure[F[_]](using F: ApplicativeError[F, Throwable]): States => F[Unit] = _ =>
    F.raiseError(SavingFailed)

  val accountRegistrationFailingState: FSM[RegistrationEither, Commands, States] =
    (command: Commands) =>
      for {
        currentState <- withEvents.toFSM(command).get
        _            <- StateT.liftF(saveState[RegistrationEither](currentState))
        // _         <- StateT.liftF(saveStateFailure[RegistrationEither](currentState))
      } yield ()

  val accountRegistrationIO: EitherT[IO, NonEmptyChain[AccountRegistrationError], Unit] =
    for {
      initialState <- loadState[RegistrationEither]
      _ <- accountRegistrationFailingState
        .traverse(Data.AccountRegistration.commandsWrongToken)
        // .traverse(Data.AccountRegistration.commandsWrongIdentity)
        // .traverse(Data.AccountRegistration.commandsNoTransition)
        .run(initialState)
    } yield ()

  override def run: IO[Unit] =
    accountRegistrationIO.leftSemiflatTap(nec => IO(println(nec))).value.map(_ => ())
}

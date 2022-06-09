package org.tp.process_time_state
package examples

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

import cats.{ Applicative, ApplicativeError }
import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.implicits.*
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.instances.either.*
import cats.syntax.either.*

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

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
        (currentState, _) <- withEvents.toFSM(command)
        _                 <- StateK.lift(saveStateFailure[RegistrationEither](currentState))
      } yield (currentState, ())

  val accountRegistrationIO: EitherT[IO, NonEmptyChain[AccountRegistrationError], Unit] =
    for {
      initialState <- loadState[RegistrationEither]
      _ <- accountRegistrationFailingState.traverse(
        Data.AccountRegistration.commands,
      )(
        initialState,
      )
    } yield ()

  override def run: IO[Unit] = accountRegistrationIO.value.map(_ => ())
}

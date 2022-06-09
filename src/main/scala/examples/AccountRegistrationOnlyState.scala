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

import cats.Applicative
import cats.data.{ EitherT, NonEmptyChain }
import cats.effect.implicits.*
import cats.effect.{ IO, IOApp }
import cats.implicits.*
import cats.instances.either.*
import cats.syntax.either.*

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

object AccountRegistrationOnlyState extends IOApp.Simple {

  def loadState[F[_]](using F: Applicative[F]): F[States] =
    F.pure(States.PotentialCustomer())

  def saveState[F[_]](using F: Applicative[F]): F[Unit] = F.unit

  val accountRegistration: FSM[RegistrationEither, Commands, States] = (command: Commands) =>
    for {
      (currentState, _) <- withEvents.toFSM(command)
      _                 <- StateK.lift(saveState[RegistrationEither])
    } yield (currentState, ())

  val accountRegistrationIO: EitherT[IO, NonEmptyChain[AccountRegistrationError], Unit] =
    for {
      initialState <- loadState[RegistrationEither]
      (currentState, _) <- accountRegistration.traverse(Data.AccountRegistration.commands)(
        initialState,
      )
      _ <- EitherT(IO(println(currentState).asRight))
    } yield ()

  override def run: IO[Unit] = accountRegistrationIO.value.map(_ => ())
}

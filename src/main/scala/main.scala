package org.tp.process_time_state

import Aggregate.*
import FST.*
import impl.AccountRegistration.*
import impl.{ AccountRegistration, AccountRegistrationWithEvents, AccountsReadModel, ErrorOr }

import cats.Applicative
import cats.implicits.*
import cats.instances.either.*

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.*

@main
def main(): Unit = {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val userId        = AccountId(UUID.randomUUID())
  val anotherUserId = AccountId(UUID.randomUUID())
  val email         = Email("test@example.org")
  val token         = Token("token")
  val anotherToken  = Token("another-token")

  val commands: List[Commands] =
    List(Commands.StartRegistration(userId, email, token), Commands.ConfirmEmail(userId, token))

  val commandsWrongIdentity: List[Commands] =
    List(
      Commands.StartRegistration(userId, email, token),
      Commands.ConfirmEmail(anotherUserId, token),
    )

  val commandsWrongToken: List[Commands] =
    List(
      Commands.StartRegistration(userId, email, token),
      Commands.ConfirmEmail(userId, anotherToken),
    )

  val commandsNoTransition: List[Commands] =
    List(Commands.StartRegistration(userId, email, token), Commands.DeleteDueToGDPR(userId))

  val simple    = AccountRegistration.simple
  val simpleFSM = simple.toFSM

  List(
    s"$simple traverse",
    Await.result(simpleFSM.traverse(commands)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      simpleFSM.traverse(commandsWrongIdentity)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      simpleFSM.traverse(commandsWrongToken)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      simpleFSM.traverse(commandsNoTransition)(States.PotentialCustomer()).value,
      1.second,
    ),
    s"$simple runAll",
    Await.result(simpleFSM.runAll(commands)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      simpleFSM.runAll(commandsWrongIdentity)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(simpleFSM.runAll(commandsWrongToken)(States.PotentialCustomer()).value, 1.second),
    Await.result(simpleFSM.runAll(commandsNoTransition)(States.PotentialCustomer()).value, 1.second),
  ).foreach(println)

  val withEvents    = AccountRegistration.withEvents
  val withEventsFSM = withEvents.toFSM
  val withEventsFST = withEvents.toFST

  List(
    s"$withEvents FSM traverse",
    Await.result(withEventsFSM.traverse(commands)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      withEventsFSM.traverse(commandsWrongIdentity)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFSM.traverse(commandsWrongToken)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFSM.traverse(commandsNoTransition)(States.PotentialCustomer()).value,
      1.second,
    ),
    s"$withEvents FST traverse",
    Await.result(withEventsFST.traverse(commands)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      withEventsFST.traverse(commandsWrongIdentity)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFST.traverse(commandsWrongToken)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFST.traverse(commandsNoTransition)(States.PotentialCustomer()).value,
      1.second,
    ),
    s"$withEvents FSM runAll",
    Await.result(withEventsFSM.runAll(commands)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      withEventsFSM.runAll(commandsWrongIdentity)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFSM.runAll(commandsWrongToken)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFSM.runAll(commandsNoTransition)(States.PotentialCustomer()).value,
      1.second,
    ),
    s"$withEvents FST runAll",
    Await.result(withEventsFST.runAll(commands)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      withEventsFST.runAll(commandsWrongIdentity)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFST.runAll(commandsWrongToken)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(
      withEventsFST.runAll(commandsNoTransition)(States.PotentialCustomer()).value,
      1.second,
    ),
  ).foreach(println)

  def storeReadModel[F[_]](using
      F: Applicative[F],
  ): AccountsReadModel.Model => F[Unit] = _ => F.pure(())

  def storeEvent[F[_]](using
      F: Applicative[F],
  ): AccountRegistrationWithEvents.Events => F[Unit] = _ => F.pure(())

  val projection = (c: Commands) =>
    for {
      (s, event)     <- withEventsFST(c)
      _              <- StateK.lift(storeEvent[ErrorOr](event))
      (_, readModel) <- StateK.lift(AccountsReadModel.makeProjection[ErrorOr](event))
      _              <- StateK.lift(storeReadModel[ErrorOr](readModel))
    } yield (s, readModel)

  List(
    s"$projection",
    Await.result(projection.runAll(commands)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      projection.runAll(commandsWrongIdentity)(States.PotentialCustomer()).value,
      1.second,
    ),
    Await.result(projection.runAll(commandsWrongToken)(States.PotentialCustomer()).value, 1.second),
    Await.result(
      projection.runAll(commandsNoTransition)(States.PotentialCustomer()).value,
      1.second,
    ),
  ).foreach(println)
}

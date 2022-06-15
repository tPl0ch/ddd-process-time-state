package org.tp.process_time_state

import cats.data.{ IndexedState, IndexedStateT, Kleisli, NonEmptyChain, StateT }
import cats.implicits.*
import cats.{ Applicative, FlatMap, Functor, Monad, MonadThrow, Monoid }
import scala.annotation.targetName

import Lifecycles.*
import Transitions.*

/** A Finite-State-Transducer (FST) is a function of a Command to StateF */
type FST[F[_], -C, S, E] = C => IndexedStateT[F, S, S, E]

/** A Finite-State-Machine (FSM) is just a FST with a Unit Output */
type FSM[F[_], -C, S] = FST[F, C, S, Unit]

object Aggregates {
  def apply[F[_], C, S, EE <: Error](behaviors: BehaviorsK[F, C, S])(using Monad[F]): FSM[F, C, S] =
    (currentCommand: C) =>
      StateT { (currentState: S) =>
        for {
          newState <- behaviors(currentCommand, currentState)
        } yield (newState, ())
      }

  def apply[F[_], C, S, E](
      behaviors: BehaviorsK[F, C, S],
  )(outputs: OutputsK[F, C, S, E])(using Monad[F]): FST[F, C, S, E] = (currentCommand: C) =>
    StateT { (currentState: S) =>
      for {
        newState <- behaviors(currentCommand, currentState)
        event    <- outputs(currentCommand, currentState)
      } yield (newState, event)
    }

  extension [F[_], C, S, E](aggregate: FST[F, C, S, E])
    def run(command: C)(state: S)(using F: FlatMap[F]): F[(S, E)] = aggregate(command).run(state)
    def runEvent(command: C)(state: S)(using F: FlatMap[F]): F[E] = aggregate(command).runA(state)
    def runState(command: C)(state: S)(using F: FlatMap[F]): F[S] = aggregate(command).runS(state)

    def runAll(commands: List[C])(state: S)(using F: Monad[F]): F[(S, List[E])] =
      aggregate.traverse(commands).run(state)

    def runAllEvents(commands: List[C])(state: S)(using F: Monad[F]): F[List[E]] =
      aggregate.traverse(commands).runA(state)

    def runAllState(commands: List[C])(state: S)(using F: Monad[F]): F[S] =
      aggregate.traverse(commands).runS(state)

    private def traverse(commands: List[C])(using F: Monad[F]): StateT[F, S, List[E]] =
      commands.foldLeft(StateT.liftF(F.pure(Nil: List[E]))) { (stateT, command) =>
        for {
          acc   <- stateT
          event <- aggregate(command)
        } yield acc.appended(event)
      }
}

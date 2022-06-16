package org.tp.process_time_state

import scala.annotation.targetName

import cats.data.StateT
import cats.implicits.*
import cats.{ FlatMap, Monad }

import Lifecycle.*
import Transitions.*

/** A Finite-State-Transducer (FST) is a function of a Command to StateF */
type FST[F[_], -C, S, E] = C => StateT[F, S, E]

/** A Finite-State-Machine (FSM) is just a FST with a Unit Output */
type FSM[F[_], -C, S] = FST[F, C, S, Unit]

extension [F[_], C, S, E](aggregate: FST[F, C, S, E])
  def run(command: C)(state: S)(using F: FlatMap[F]): F[(S, E)] = aggregate(command).run(state)
  def runEvent(command: C)(state: S)(using F: FlatMap[F]): F[E] = aggregate(command).runA(state)
  def runState(command: C)(state: S)(using F: FlatMap[F]): F[S] = aggregate(command).runS(state)

  def runAll(commands: Seq[C])(state: S)(using F: Monad[F]): F[(S, LazyList[E])] =
    aggregate.traverse(commands).run(state)

  def runAllEvents(commands: Seq[C])(state: S)(using F: Monad[F]): F[LazyList[E]] =
    aggregate.traverse(commands).runA(state)

  def runAllState(commands: Seq[C])(state: S)(using F: Monad[F]): F[S] =
    aggregate.traverse(commands).runS(state)

  private def traverse(commands: Seq[C])(using F: Monad[F]): StateT[F, S, LazyList[E]] =
    commands.foldLeft(StateT.liftF(F.pure(LazyList.empty[E]))) { (stateT, command) =>
      for {
        acc <- stateT
        stateF = aggregate(command)
        event <- stateF
      } yield acc.appended(event)
    }

object Aggregate {
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

}

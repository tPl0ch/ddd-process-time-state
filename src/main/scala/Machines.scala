package org.tp.process_time_state

import cats.data.{ IndexedState, IndexedStateT, Kleisli, StateT }
import cats.implicits.*
import cats.{ FlatMap, Functor, Monad, MonadThrow }
import scala.annotation.targetName

import Lifecycle.*
import Transitions.*

/** A Finite-State-Transducer (FST) is a function of an Input to StateF */
type FST[F[_], -C, S, E] = C => StateT[F, S, E]

/** A Finite-State-Machine (FSM) is just a FST with a Unit Output */
type FSM[F[_], -C, S] = FST[F, C, S, Unit]

object Machines {
  def apply[F[_], C, S](
      behaviors: BehaviorsK[F, C, S],
  )(using F: MonadThrow[F], isFinal: HasEnded[S]): FSM[F, C, S] = (currentCommand: C) =>
    StateT { (currentState: S) =>
      for {
        newState <- behaviors.onlyWhenLifecycleIsActive(currentCommand, currentState)
      } yield (newState, ())
    }

  def apply[F[_], C, S, E](behaviors: BehaviorsK[F, C, S])(
      outputs: OutputsK[F, C, S, E],
  )(using F: MonadThrow[F], isFinal: HasEnded[S]): FST[F, C, S, E] = (currentCommand: C) =>
    StateT { (currentState: S) =>
      for {
        newState <- behaviors.onlyWhenLifecycleIsActive(currentCommand, currentState)
        event    <- outputs(currentCommand, currentState)
      } yield (newState, event)
    }

  extension [F[_], C, S, E](transducer: FST[F, C, S, E])
    def run(command: C)(state: S)(using F: FlatMap[F]): F[E] =
      F.map(transducer(command).run(state))((_, e) => e)

    def runAll(commands: List[C])(state: S)(using F: Monad[F]): F[List[E]] =
      for {
        (_, acc) <- transducer.traverse(commands).run(state)
      } yield acc

    def traverse(commands: List[C])(using F: Monad[F]): StateT[F, S, List[E]] =
      commands.foldLeft(StateT.liftF(F.pure(Nil: List[E]))) { (stateT, command) =>
        for {
          acc   <- stateT
          event <- transducer(command)
        } yield acc.appended(event)
      }
}

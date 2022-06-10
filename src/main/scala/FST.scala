package org.tp.process_time_state

import cats.data.{ Kleisli, StateT }
import cats.implicits.*
import cats.{ FlatMap, Functor, Monad }

import scala.annotation.targetName

/** A Finite-State-Transducer (FST) is a function of an Input to StateF */
final type FST[F[_], In, State, Out] = In => StateT[F, State, Out]

/** A Finite-State-Machine (FSM) is just a FST with a Unit Output */
final type FSM[F[_], In, State] = FST[F, In, State, Unit]

object FST {
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

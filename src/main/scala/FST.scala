package org.tp.process_time_state

import StateK.lift

import cats.data.Kleisli
import cats.implicits.*
import cats.{ FlatMap, Functor, Monad }

import scala.annotation.targetName

/** A Finite-State-Transducer (FST) is a function of an Input to StateF */
final type FST[F[_], In, State, Out] = In => StateK[F, State, Out]

/** A Finite-State-Machine (FSM) is just a FST with a Unit Output */
final type FSM[F[_], In, State] = FST[F, In, State, Unit]

object FST {
  extension [F[_], C, S, E](transducer: FST[F, C, S, E])
    def run(command: C)(state: S)(using F: Functor[F]): F[E] =
      F.map(transducer(command)(state))((_, e) => e)

    def runAll(commands: List[C])(state: S)(using F: Monad[F]): F[List[E]] =
      for {
        (_, acc) <- transducer.traverse(commands)(state)
      } yield acc

    def traverse(commands: List[C])(using F: Monad[F]): StateK[F, S, List[E]] =
      commands.foldLeft(lift[F, S, List[E]](Nil)) { (stateK, command) =>
        for {
          (currentState, acc) <- stateK
          (newState, event)   <- StateK.set(transducer(command)(currentState))
        } yield (newState, acc.appended(event))
      }

    def compose[S1, E1](other: FST[F, E, S1, E1])(using F: FlatMap[F]): FST[F, C, (S, S1), E1] =
      (command: C) =>
        Kleisli { (s1: S, s2: S1) =>
          for {
            (given S, given E)   <- transducer(command)(s1)
            (given S1, given E1) <- other(summon[E])(s2)
          } yield ((summon[S], summon[S1]), summon[E1])
        }
}

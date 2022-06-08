package org.tp.process_time_state

import StateF.*

import cats.{ FlatMap, Monad }
import cats.implicits.*

import scala.annotation.targetName

/** A Finite-State-Transducer (FST) is a function of an Input to StateF */
final type FST[F[_], In, State, Out] = In => StateF[F, State, Out]

/** A Finite-State-Machine (FSM) is just a FST with a Unit Output */
final type FSM[F[_], In, State] = FST[F, In, State, Unit]

object FST {
  extension [F[_], C, S](machine: FST[F, C, S, Unit])
    @targetName("fsm")
    def run(commands: List[C])(using F: Monad[F]): StateF[F, S, Unit] =
      StateF.traverse(commands)(machine).map(_ => ())

  extension [F[_], C, S, E](transducer: FST[F, C, S, E])
    @targetName("fst")
    def run(commands: List[C])(using F: Monad[F]): StateF[F, S, List[E]] =
      StateF.traverse(commands)(transducer)
}

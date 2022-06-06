package org.tp.process_time_state

import StateF.*

import cats.{ Applicative, FlatMap, Monad }
import cats.data.Kleisli
import cats.implicits.*

trait States[F[_]] { self: Aggregate[F] =>
  final def state(using flatMap: FlatMap[F]): C => StateF[F, S, E] =
    (currentCommand: C) =>
      (currentState: S) =>
        for {
          newState     <- transitions((currentCommand, currentState))
          outputSignal <- events((currentCommand, currentState))
        } yield (newState, outputSignal)

  final def traverse(commands: List[C])(using F: Monad[F]): StateF[F, S, List[E]] =
    StateF.traverse(commands)(state(_))
}

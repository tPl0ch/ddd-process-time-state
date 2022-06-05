package org.tp.process_time_state

import cats.FlatMap
import cats.implicits.*

trait States[F[_]] { self: Aggregate[F] =>
  final type StateF = LabelIn => LabelOutF

  final def state(using flatMap: FlatMap[F]): StateF =
    (currentCommand: C, currentState: S) =>
      for {
        newState     <- transitions((currentCommand, currentState))
        outputSignal <- events((currentCommand, currentState))
      } yield (newState, outputSignal)

  extension (underlying: StateF) final def run(l: LabelIn): LabelOutF = underlying(l)
}

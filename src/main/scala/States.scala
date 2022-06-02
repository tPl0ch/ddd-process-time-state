package org.tp
package process_time_state

import cats.FlatMap
import cats.implicits.*

trait States[F[_]] { self: Aggregate[F] =>
  type StateF = LabelIn => LabelOutF

  // TODO: Undefined transition/event handling
  final def state(using flatMap: FlatMap[F]): StateF =
    (currentCommand: C, currentState: S) =>
      for {
        newState     <- transitions((currentCommand, currentState))
        outputSignal <- events((currentCommand, currentState))
      } yield (newState, outputSignal)

  extension (underlying: StateF) def run(l: LabelIn): LabelOutF = underlying(l)
}

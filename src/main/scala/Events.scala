package org.tp
package process_time_state

trait Events[F[_]] { self: Aggregate[F] =>
  type EventF = PartialFunction[LabelIn, F[E]]

  def events: EventF
}

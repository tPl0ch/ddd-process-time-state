package org.tp.process_time_state

import cats.ApplicativeError
import cats.data.{ Kleisli, NonEmptyList }

trait Events[F[_]] { self: Aggregate[F] =>
  type Event  = PartialFunction[LabelIn, F[E]]
  type EventF = Kleisli[F, LabelIn, E]

  def events: EventF

  final case class EventNotDefined(l: LabelIn) extends DomainError {
    override def msg: String = s"Event for input label $l is not defined."
  }

  final protected def mkEventsF(
      eventsToBeLifted: Event,
  )(using
      applicativeError: ApplicativeError[F, NEL],
  ): EventF = Kleisli { (l: LabelIn) =>
    if !eventsToBeLifted.isDefinedAt(l) then
      applicativeError.raiseError(NonEmptyList.of(EventNotDefined(l).asInstanceOf[EE]))
    else eventsToBeLifted(l)
  }
}

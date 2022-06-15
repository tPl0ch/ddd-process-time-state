package org.tp.process_time_state

import cats.Applicative

object EventSourcing {
  def reconstituteState[F[_], S, E](f: (E, S) => S)(snapshot: S)(events: List[E])(using
      F: Applicative[F],
  ): F[S] = events.foldLeft(F.pure(snapshot))((fs, e) => F.map(fs)(s => f(e, s)))
}

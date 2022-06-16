package org.tp.process_time_state

import cats.MonadError
import cats.data.NonEmptyChain

object EventSourcing {
  def reconstituteState[F[_], S, E, EE <: Error](
      f: (E, S) => InvariantError[EE, S],
  )(snapshot: S)(events: Seq[E])(using
      M: MonadError[F, NonEmptyChain[EE]],
  ): F[S] =
    events.foldLeft(M.pure(snapshot))((fs, e) =>
      M.flatMap(fs)(s => f(e, s).fold(M.raiseError, M.pure)),
    )

  final case class CannotReconstituteFrom[E, S](e: E, s: S) extends Error {
    override def msg: String = s"Cannot reconstitute from Event $e and state $s"
  }
}

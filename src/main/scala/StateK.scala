package org.tp.process_time_state

import cats.Applicative
import cats.data.Kleisli
import cats.implicits.*

final type StateK[F[_], S, A] = Kleisli[F, S, (S, A)]

object StateK {
  def unit[F[_], S, A](a: A)(using F: Applicative[F]): StateK[F, S, A] =
    Kleisli(s => F.pure((s, a)))
}

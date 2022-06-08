package org.tp.process_time_state

import cats.data.Kleisli
import cats.implicits.*
import cats.{ Applicative, FlatMap, Functor, Monad }

final type StateK[F[_], S, A] = Kleisli[F, S, (S, A)]

object StateK {
  def unit[F[_], S, A](a: A)(using F: Applicative[F]): StateK[F, S, A] =
    Kleisli(s => F.pure((s, a)))

  def traverse[F[_], S, C, E](commands: List[C])(f: C => StateK[F, S, E])(using
      F: Monad[F],
  ): StateK[F, S, List[E]] =
    commands
      .foldLeft(unit[F, S, List[E]](Nil)) { (k, command) =>
        k.flatMap { (s, acc) =>
          Kleisli { _ => F.map(f(command)(s))((s1, e) => (s1, acc.appended(e))) }
        }
      }
}

package org.tp.process_time_state

import cats.implicits.*
import cats.{ Applicative, FlatMap, Functor, Id, Monad }

type StateF[F[_], S, A] = S => F[(S, A)]

object StateF {

  extension [F[_], S, A](underlying: StateF[F, S, A])
    def run(s: S): F[(S, A)] = underlying(s)

    def flatMap[B](f: A => StateF[F, S, B])(using F: FlatMap[F]): StateF[F, S, B] =
      s => F.flatMap(underlying(s)) { case (s1, a) => f(a)(s1) }

    def map[B](f: A => B)(using F: Functor[F]): StateF[F, S, B] = s =>
      F.map(underlying(s)) { case (s1, a) => (s1, f(a)) }

    def map2[B, C](sb: StateF[F, S, B])(f: (A, B) => C)(using F: FlatMap[F]): StateF[F, S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

  def unit[F[_], S, A](a: A)(using F: Applicative[F]): StateF[F, S, A] = s => F.pure((s, a))

  def get[F[_], S](using F: Applicative[F]): StateF[F, S, S] = s => F.pure((s, s))

  def set[F[_], S](fs: F[S])(using F: Functor[F]): StateF[F, S, Unit] = _ => F.map(fs)((_, ()))

  def modify[F[_], S](f: S => F[S])(using F: Monad[F]): StateF[F, S, Unit] =
    for
      s <- get       // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    yield ()

  def traverse[F[_], S, A, B](as: List[A])(f: A => StateF[F, S, B])(using
      F: Monad[F],
  ): StateF[F, S, List[B]] =
    as.foldRight(unit[F, S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[F[_], S, A](actions: List[StateF[F, S, A]])(using
      F: Monad[F],
  ): StateF[F, S, List[A]] =
    actions.foldRight(unit[F, S, List[A]](Nil))((f, acc) => f.map2(acc)(_ :: _))
}

package org.tp.process_time_state

import cats.data.Kleisli
import cats.implicits.*
import cats.{ Applicative, Functor, Monad }

import scala.annotation.targetName

final type StateK[F[_], S, A] = Kleisli[F, S, (S, A)]

object StateK {
  def unit[F[_], A](a: A)(using F: Applicative[F]): StateK[F, Unit, A] =
    Kleisli(_ => F.pure(((), a)))

  def set[F[_], S, A](fas: F[(S, A)]): StateK[F, S, A] = Kleisli(_ => fas)

  def lift[F[_], S, A](a: A)(using F: Applicative[F]): StateK[F, S, A] =
    Kleisli(s => F.pure((s, a)))

  def lift[F[_], S, A](fa: F[A])(using F: Functor[F]): StateK[F, S, A] =
    Kleisli(s => F.map(fa)((s, _)))
}

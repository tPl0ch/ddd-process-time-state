package org.tp.process_time_state

import cats.{ FlatMap, Functor, Id as CatsId }

object machines {

  opaque type StateF[F[_], S, E] = S => F[(S, E)]
  type State[S, E]               = StateF[CatsId, S, E]

  opaque type MachineF[F[_], C, S, E] = C => StateF[F, S, E]

  object StateF {
    def apply[F[_], S, E](f: S => F[(S, E)]): StateF[F, S, E] = f

    def apply[F[_]: FlatMap: Functor, S, E](b: S => F[S])(o: S => F[E]): StateF[F, S, E] =
      (s: S) => FlatMap[F].flatMap(b(s))(ss => Functor[F].map(o(s))(e => (ss, e)))

    def unit[F[_]: Functor, S, E](fe: F[E]): StateF[F, S, E] =
      (s: S) => Functor[F].map(fe)((s, _))
  }

  object MachineF {
    def apply[F[_], C, S, E](b: C => S => F[S])(o: C => S => F[E]): MachineF[F, C, S, E] = ???
  }
}

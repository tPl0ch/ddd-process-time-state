package org.tp.process_time_state

import cats.MonadError
import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ Kleisli, NonEmptyChain }
import cats.implicits.*
import cats.syntax.validated.*
import cats.{ ApplicativeError, ApplicativeThrow, MonadThrow }
import scala.annotation.targetName

import Invariants.*
import Lifecycle.{ IsEnd, LifecycleHasEnded }

type Transition[-C, -S, E, EE <: Error] = PartialFunction[(C, S), InvariantError[EE, E]]
type TransitionK[F[_], -C, -S, E]       = Kleisli[F, (C, S), E]

object Transitions {

  extension [F[_], C, S](transition: TransitionK[F, C, S, S])
    def withLifecycleCheck(using
        F: ApplicativeThrow[F],
        isFinal: IsEnd[S],
    ): TransitionK[F, C, S, S] =
      Kleisli { (c: C, s: S) =>
        if !isFinal(s) then transition((c, s))
        else F.raiseError(LifecycleHasEnded(c, s))
      }

  extension [C, S, E, EE <: Error](transition: Transition[C, S, E, EE])

    def guard(
        invariants: List[Invariant[C, S, EE]] = Nil,
    )(using E: (C, S) => EE): Transition[C, S, E, EE] = { case (c: C, s: S) =>
      if !transition.isDefinedAt((c, s)) then NonEmptyChain.one(E(c, s)).invalid
      else
        invariants
          .map(f => f.check((c, s)))
          .sequence
          .andThen(_ => transition(c, s))
    }

    @targetName("withGuards")
    /** Guards a Transition with a List of Invariants */
    def <<<(invariants: List[Invariant[C, S, EE]])(using
        E: (C, S) => EE,
    ): Transition[C, S, E, EE] =
      guard(invariants)

    @targetName("withGuard")
    /** Guards a Transition with single Invariant */
    def <<(invariant: Invariant[C, S, EE])(using
        E: (C, S) => EE,
    ): Transition[C, S, E, EE] =
      guard(List(invariant))

    def liftF[F[_]](using
        F: ApplicativeError[F, NonEmptyChain[EE]],
        E: (C, S) => EE,
    ): TransitionK[F, C, S, E] =
      Kleisli { (c: C, s: S) =>
        if !transition.isDefinedAt((c, s)) then F.raiseError(NonEmptyChain.one(E(c, s)))
        else transition((c, s)).fold(F.raiseError, F.pure)
      }
}

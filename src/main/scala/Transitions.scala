package org.tp.process_time_state

import cats.ApplicativeError
import cats.data.{ Kleisli, NonEmptyChain }
import cats.data.Validated.{ Invalid, Valid }
import cats.implicits.*
import cats.syntax.validated.*

import scala.annotation.targetName

type Transition[-C, -S, E]        = PartialFunction[(C, S), E]
type TransitionF[F[_], -C, -S, E] = PartialFunction[(C, S), F[E]]
type TransitionK[F[_], -C, -S, E] = Kleisli[F, (C, S), E]

object Transitions {

  /** This error is indicated when there is no TransitionF for a LabelIn.
    */
  case class TransitionNotDefined[C, S](c: C, s: S) extends Throwable {
    override def getMessage: String = s"Transition is not defined for command $c and state $s"
  }

  extension [C, S, E](transition: Transition[C, S, E])

    def guard[F[_], EE <: Throwable](
        invariants: List[Invariant[C, S, EE]] = Nil,
    )(using F: ApplicativeError[F, NonEmptyChain[EE]]): TransitionK[F, C, S, E] = Kleisli {
      (c: C, s: S) =>
        {
          invariants
            .map(f => f.check((c, s)))
            .sequence match
            case Valid(_)     => transition.liftF(c, s)
            case Invalid(nec) => F.raiseError(nec)
        }
    }

    @targetName("withGuards")
    /** Guards a Transition with a List of Invariants */
    final def <<<[F[_], EE <: Throwable](invariants: List[Invariant[C, S, EE]])(using
        F: ApplicativeError[F, NonEmptyChain[EE]],
    ): TransitionK[F, C, S, E] =
      guard(invariants)

    @targetName("withGuard")
    /** Guards a Transition with single Invariant */
    final def <<[F[_], EE <: Throwable](invariant: Invariant[C, S, EE])(using
        F: ApplicativeError[F, NonEmptyChain[EE]],
    ): TransitionK[F, C, S, E] =
      guard(List(invariant))

    def liftF[F[_], EE <: Throwable](using
        F: ApplicativeError[F, NonEmptyChain[EE]],
        e: (C, S) => EE,
    ): TransitionF[F, C, S, E] =
      (c: C, s: S) =>
        if !transition.isDefinedAt((c, s)) then F.raiseError(NonEmptyChain.one(e(c, s)))
        else F.pure(transition((c, s)))

}

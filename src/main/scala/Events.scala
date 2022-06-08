package org.tp.process_time_state

import identity.HasIdentity

import cats.data.Kleisli
import cats.implicits.*
import cats.{ ApplicativeError, FlatMap, Monad }

import scala.annotation.targetName

/** This trait provides the Event output types and functions */
trait Events[F[_]] { self: Aggregate[F] =>

  /** The Event alphabet type as a subtype of HasIdentity[ID] */
  type E <: HasIdentity[ID]

  /** Events are modelled as a partial function of a LabelIn to an Event */
  final type Outputs = PartialFunction[LabelIn, F[E]]

  /** We again lift the partial functions into a Kleisli representing an fn LabelIn => F[E] */
  final type OutputsF = Kleisli[F, LabelIn, E]

  /** Provides the output Events for the Aggregate
    *
    * @return
    *   The lifted partial output function that provides the Aggregate's Events within the context F
    */
  def events: OutputsF

  /** This error indicates when there is no Event defined for a valid Transition */
  final case class OutputNotDefined(l: LabelIn) extends DomainError {
    override def msg: String = s"Event for input label $l is not defined."
  }

  extension (outputs: Outputs)
    @targetName("maybeOutputs")
    /** Makes calling the partial output function safe by either raising an applicative error if it
      * is not defined for a (command, state) label or running the function
      */
    final def maybe(using F: ApplicativeError[F, NEC]): Outputs =
      Transitions.maybe(outputs, l => OutputNotDefined(l).asInstanceOf[EE])

    @targetName("liftOutputs")
    /** Lifts an Events output function into a Kleisli */
    final def liftK(using F: ApplicativeError[F, NEC]): OutputsF = Kleisli { maybe(_) }
}

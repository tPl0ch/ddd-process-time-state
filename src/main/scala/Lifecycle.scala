package org.tp.process_time_state

import Lifecycle.NotStarted

/** The UID[ID] type represents a Union of the Aggregate's concrete ID type and the PreGenesis
  * state.
  */
type UID[ID] = ID | NotStarted.type

/** A function that determines if a certain state is a final state. */
final type HasEnded[S] = S => Boolean

/** This trait needs to be implemented by the Command, State and Even alphabets and provides the
  * Aggregate identity. This identity can be defined from the the outside and needs to be
  * accompanied by an EqualIdentities[ID] given instance if you want to leverage the identity guard.
  *
  * An implementer can decide if a concrete ID type or the NoIdentitySet singleton should be
  * returned.
  */
trait Lifecycle[ID] {
  def id: UID[ID]
}

object Lifecycle {

  /** Represents a state before any lifecycle has started. */
  case object NotStarted

  /** This error indicates when the Aggregate is in a final state and can't process any more
    * commands
    */
  final case class LifecycleHasEnded[C, S](c: C, s: S) extends Error {
    override def msg: String =
      s"Command $c cannot be processed because state $s is final"
  }
}

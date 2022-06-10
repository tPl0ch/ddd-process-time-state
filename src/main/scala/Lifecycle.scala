package org.tp.process_time_state

object Lifecycle {

  /** This singleton represents an initial state that does not yet have an identity. */
  case object NoId

  /** A function that determines if a certain state is a final state. */
  final type IsEnd[S] = S => Boolean

  /** A function that determines if a certain state is an initial state. */
  final type IsBeginning[S] = S => Boolean

  /** This error indicates when the Aggregate is in a final state and can't process any more
    * commands
    */
  final case class LifecycleHasEnded[C, S](c: C, s: S) extends Throwable {
    override def getMessage: String =
      s"Command $c cannot be processed because state $s is final"
  }
}

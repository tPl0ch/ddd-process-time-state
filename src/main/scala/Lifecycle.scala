package org.tp.process_time_state

object Lifecycle {

  /** Represents a state before any lifecycle has started. */
  case object NotStarted

  /** A function that determines if a certain state is a final state. */
  final type HasEnded[S] = S => Boolean

  /** A function that determines if a certain state is an initial state. */
  final type IsRunning[S] = S => Boolean

  /** This error indicates when the Aggregate is in a final state and can't process any more
    * commands
    */
  final case class LifecycleHasEnded[C, S](c: C, s: S) extends Throwable {
    override def getMessage: String =
      s"Command $c cannot be processed because state $s is final"
  }
}

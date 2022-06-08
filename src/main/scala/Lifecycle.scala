package org.tp.process_time_state

object Lifecycle {

  /** This singleton represents an initial state that does not yet have an identity. */
  case object PreGenesis

  /** A function that determines if a certain state is a final state. */
  final type IsEnd[S] = S => Boolean

  /** A function that determines if a certain state is an initial state. */
  final type IsBeginning[S] = S => Boolean
}

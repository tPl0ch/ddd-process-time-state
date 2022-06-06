package org.tp.process_time_state

object Lifecycle {

  /** This singleton represents an initial state that does not yet have an identity */
  case object PreGenesis

  final type IsEnd[S]       = S => Boolean
  final type IsBeginning[S] = S => Boolean
}

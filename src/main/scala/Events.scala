package org.tp.process_time_state

object Events {

  /** This error indicates when there is no Event defined for a valid Transition */
  final case class EventNotDefined[C, S](c: C, s: S) extends Error {
    override def msg: String = s"Event for input label ($c, $s) is not defined."
  }
}

package org.tp.process_time_state

/** A marker interface for the supported errors. */
trait Error extends Throwable {
  def msg: String
  override def getMessage: String = msg
}

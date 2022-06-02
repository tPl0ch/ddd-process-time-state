package org.tp
package process_time_state

trait DomainError extends Throwable {
  def msg: String
  override def getMessage: String = msg
}

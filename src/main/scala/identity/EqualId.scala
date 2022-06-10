package org.tp.process_time_state
package identity

/** This trait is a comparison type-class for a specific Aggregate ID that you have chosen. */
trait EqualId[ID] {
  def equals(idA: UID[ID], idB: UID[ID]): Boolean
}

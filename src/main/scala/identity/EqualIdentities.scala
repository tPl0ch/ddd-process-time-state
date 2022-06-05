package org.tp.process_time_state
package identity

/** This trait is a comparison type-class for a specific Aggregate ID that you have chosen. */
trait EqualIdentities[ID] {
  def equals(idA: ID | PreGenesis.type, idB: ID | PreGenesis.type): Boolean
}

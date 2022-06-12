package org.tp.process_time_state
package identity

import Identities.UID

/** This trait is a comparison type-class for a specific Aggregate ID that you have chosen. */
trait ComparesLifecycles[ID] {
  def equals(idA: Lifecycle[ID], idB: Lifecycle[ID]): Boolean
}

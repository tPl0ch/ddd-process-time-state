package org.tp.process_time_state
package identity

import Lifecycle.NoId

/** The UID[ID] type represents a Union of the Aggregate's concrete ID type and the PreGenesis
  * state.
  */
final type UID[ID] = ID | NoId.type

/** This trait needs to be implemented by the Command, State and Even alphabets and provides the
  * Aggregate identity. This identity can be defined from the the outside and needs to be
  * accompanied by an EqualIdentities[ID] given instance if you want to leverage the identity guard.
  *
  * An implementer can decide if a concrete ID type or the NoIdentitySet singleton should be
  * returned.
  */
trait HasId[ID] {
  def id: UID[ID]
}

package org.tp.process_time_state
package domain.registration

import Identities.UID
import Lifecycle.{ IsEnd, NoId }
import domain.registration.Model.{ AccountId, State }
import identity.EqualId

object Givens {
  given isFinalState: IsEnd[State] with
    override def apply(s: State): Boolean = s match
      case _: State.Deleted => true
      case _                => false

  given accountIdEquals: EqualId[AccountId] with
    override def equals(idA: UID[AccountId], idB: UID[AccountId]): Boolean =
      (idA, idB) match
        case (a: AccountId, b: AccountId) => a.id.equals(b.id)
        case (_: NoId.type, _)            => false // Commands should always have an identity
        case (_, _: NoId.type)            => true  // If there is a pre-genesis state, allow
        case _                            => false
}

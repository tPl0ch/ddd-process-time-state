package org.tp.process_time_state
package domain.profile

import Identities.*
import Lifecycles.*
import domain.profile.Errors.*
import domain.profile.Model.*
import domain.profile.Types.*

object Givens {
  given isFinalState: Lifecycles.HasEnded[S] with
    override def apply(s: S): Boolean = s match
      case _: State.DeletedProfile => true
      case _                       => false

  given accountIdEquals: ComparesLifecycles[ID] with
    override def equals(idA: Lifecycle[ID], idB: Lifecycle[ID]): Boolean =
      (idA.id, idB.id) match
        case (a: ProfileId, b: ProfileId)       => a.id.equals(b.id)
        case (_: Lifecycles.NotStarted.type, _) => false // Commands should always have an identity
        case (_, _: Lifecycles.NotStarted.type) => true  // If there is a pre-genesis state, allow
}

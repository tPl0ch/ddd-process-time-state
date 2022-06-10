package org.tp.process_time_state
package domain.profile

import Identities.*
import Lifecycle.*
import domain.profile.Errors.*
import domain.profile.Model.*
import domain.profile.Types.*
import identity.*

object Givens {
  given isFinalState: IsEnd[S] with
    override def apply(s: S): Boolean = s match
      case _: State.DeletedProfile => true
      case _                       => false

  given accountIdEquals: EqualId[ID] with
    override def equals(idA: UID[ID], idB: UID[ID]): Boolean =
      (idA, idB) match
        case (a: ProfileId, b: ProfileId) => a.id.equals(b.id)
        case (_: NoId.type, _)            => false // Commands should always have an identity
        case (_, _: NoId.type)            => true  // If there is a pre-genesis state, allow
}

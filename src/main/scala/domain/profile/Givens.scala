package org.tp.process_time_state
package domain.profile

import cats.kernel.Eq

import domain.profile.Errors.*
import domain.profile.Model.*
import domain.profile.Types.*
import domain.registration.Model.AccountId

object Givens {
  given isFinalState: HasEnded[S] with
    override def apply(s: S): Boolean = s match
      case _: State.DeletedProfile => true
      case _                       => false

  given accountIdEquals: Eq[Lifecycle[ProfileId]] with
    override def eqv(x: Lifecycle[ProfileId], y: Lifecycle[ProfileId]): Boolean =
      (x.id, y.id) match
        case (a: ProfileId, b: ProfileId) => a.id.equals(b.id)
        case (_: NotStarted.type, _)      => false // Commands should always have an identity
        case (_, _: NotStarted.type)      => true  // If there is a pre-genesis state, allow
}

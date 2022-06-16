package org.tp.process_time_state
package domain.profile

import cats.kernel.Eq

import Errors.*
import Model.*
import Types.*
import domain.registration.Model.AccountId

object Givens {
  given isFinalState: HasEnded[S] with
    override def apply(s: S): Boolean = s match
      case _: State.DeletedProfile => true
      case _                       => false

  given accountIdEquals: Eq[Lifecycle[ID]] with
    override def eqv(x: Lifecycle[ID], y: Lifecycle[ID]): Boolean =
      (x.id, y.id) match
        case (a: ID, b: ID)          => a.id.equals(b.id)
        case (_: NotStarted.type, _) => false // Commands should always have an identity
        case (_, _: NotStarted.type) => true  // If there is a pre-genesis state, allow
}

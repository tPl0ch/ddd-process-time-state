package org.tp.process_time_state
package domain.registration

import cats.kernel.Eq

import Model.State
import Types.*

object Givens {
  given isFinalState: HasEnded[S] with
    override def apply(s: S): Boolean = s match
      case _: State.Active => true
      case _               => false

  given compareAccounts: Eq[Lifecycle[ID]] with
    override def eqv(x: Lifecycle[ID], y: Lifecycle[ID]): Boolean =
      (x.id, y.id) match
        case (a: ID, b: ID)                   => a.id.equals(b.id)
        case (_: LifecycleNotStarted.type, _) => false // Commands should always have an identity
        case (_, _: LifecycleNotStarted.type) => true  // If there is a pre-genesis state, allow
        case _                                => false
}

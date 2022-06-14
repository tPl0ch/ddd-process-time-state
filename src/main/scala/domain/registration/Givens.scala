package org.tp.process_time_state
package domain.registration

import Lifecycle.{ HasEnded, NotStarted }
import domain.registration.Model.{ AccountId, State }
import identity.{ ComparesLifecycles, Lifecycle }

object Givens {
  given isFinalState: HasEnded[State] with
    override def apply(s: State): Boolean = s match
      case _: State.Active => true
      case _               => false

  given compareAccounts: ComparesLifecycles[AccountId] with
    override def equals(accountA: Lifecycle[AccountId], accountB: Lifecycle[AccountId]): Boolean =
      (accountA.id, accountB.id) match
        case (a: AccountId, b: AccountId) => a.id.equals(b.id)
        case (_: NotStarted.type, _)      => false // Commands should always have an identity
        case (_, _: NotStarted.type)      => true  // If there is a pre-genesis state, allow
        case _                            => false
}

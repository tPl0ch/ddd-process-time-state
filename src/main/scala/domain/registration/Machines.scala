package org.tp.process_time_state
package domain.registration

import Behaviors.*
import Events.*
import Types.*

object Machines {
  val registrationStateMachine: StateMachine = Aggregate(behaviors)
  val registrationTransducer: Transducer     = Aggregate(behaviors)(events)
}

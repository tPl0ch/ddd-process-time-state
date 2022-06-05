package org.tp.process_time_state

trait Machine[F[_]] { self: Aggregate[F] =>
  type MachineF = C => LabelOutF
}

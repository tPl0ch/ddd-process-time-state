package org.tp.process_time_state

import Lifecycle.{ IsBeginning, IsEnd }

trait Lifecycles[F[_]] { self: Aggregate[F] => }

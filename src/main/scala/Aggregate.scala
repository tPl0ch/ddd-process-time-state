package org.tp.process_time_state

import cats.data.StateT
import cats.implicits.*
import cats.{ FlatMap, Monad }

import identity.*

trait Aggregate[F[_]] extends Identities[F] with Transitions[F] with Invariants[F] {

  /** The Command alphabet type as a subtype of HasIdentity[ID] */
  type C <: HasId[ID]

  /** The State alphabet type as a subtype of HasIdentity[ID] */
  type S <: HasId[ID]

  /** The Domain Error type as a subtype of DomainError */
  type EE <: DomainError
}

object Aggregate {

  extension [F[_], T <: Aggregate[F]](a: T)
    def toFSM(using F: Monad[F]): FSM[F, a.C, a.S] = (currentCommand: a.C) =>
      StateT { (currentState: a.S) =>
        for {
          newState <- a.transitions((currentCommand, currentState))
        } yield (newState, ())
      }

  extension [F[_], T <: Aggregate[F] & Events[F]](a: T)
    def toFST(using F: Monad[F]): FST[F, a.C, a.S, a.E] = (currentCommand: a.C) =>
      StateT { (currentState: a.S) =>
        for {
          newState <- a.transitions((currentCommand, currentState))
          event    <- a.events((currentCommand, currentState))
        } yield (newState, event)
      }
}

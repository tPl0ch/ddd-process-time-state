package org.tp.process_time_state

import identity.*

import cats.FlatMap
import cats.implicits.*

trait Aggregate[F[_]] extends Identities[F] with Transitions[F] {

  /** The Command alphabet type as a subtype of HasIdentity[ID] */
  type C <: HasIdentity[ID]

  /** The State alphabet type as a subtype of HasIdentity[ID] */
  type S <: HasIdentity[ID]

  /** The Domain Error type as a subtype of DomainError */
  type EE <: DomainError
}

object Aggregate {

  extension [F[_], T <: Aggregate[F]](a: T)
    def toFSM(using F: FlatMap[F]): FSM[F, a.C, a.S] = (c: a.C) =>
      (s: a.S) =>
        for {
          newState <- a.transitions((c, s))
        } yield (newState, ())

  extension [F[_], T <: Aggregate[F] & Events[F]](a: T)
    def toFST(using F: FlatMap[F]): FST[F, a.C, a.S, a.E] = (currentCommand: a.C) =>
      (currentState: a.S) =>
        for {
          newState <- a.transitions((currentCommand, currentState))
          event    <- a.events((currentCommand, currentState))
        } yield (newState, event)

  /** An FSM is an FST with a Unit output */
  /*
  def FSM[F[_], T <: Aggregate[F]](a: T)(using F: FlatMap[F]): FST[F, a.C, a.S, Unit] =
    (currentCommand: a.C) =>
      (currentState: a.S) =>
        for {
          newState <- a.transitions((currentCommand, currentState))
        } yield (newState, ())
   */

  /** The FST uses both transitions and events to produce the valid output */
  /*  def FST[F[_], T <: Aggregate[F] & Events[F]](
      a: T,
  )(using F: FlatMap[F]): FST[F, a.C, a.S, a.E] =
    (currentCommand: a.C) =>
      (currentState: a.S) =>
        for {
          newState <- a.transitions((currentCommand, currentState))
          event    <- a.events((currentCommand, currentState))
        } yield (newState, event)*/
}

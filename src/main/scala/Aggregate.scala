package org.tp
package process_time_state

import cats.data.Validated.*
import cats.data.{ NonEmptyChain, NonEmptyList, Validated, ValidatedNel }
import cats.{ Applicative, ApplicativeError, FlatMap, Functor }
import cats.implicits.*

import scala.annotation.targetName

trait Aggregate[F[_]] extends Transitions[F] with Events[F] with States[F] {
  type ID
  type C <: HasIdentity[ID]
  type S <: HasIdentity[ID]
  type E <: HasIdentity[ID]
  type EE <: DomainError
  type NEC = NonEmptyList[EE]

  type LabelIn   = (C, S)
  type LabelOutF = F[(S, E)]
}

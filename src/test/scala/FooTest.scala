package org.hablapps.azucar

import org.scalatest._

import macros.foo

// class optic extends scala.annotation.StaticAnnotation
class derived extends scala.annotation.StaticAnnotation

import scalaz.~>
import scalaz.syntax.id._
trait Iso[TC[_[_]]] {
  type ADT[_]
  type Ev[_[_]]

  def to[P[_]](fp: TC[P]): ADT ~> P
  def from[P[_]: Ev](gp: ADT ~> P): TC[P]
}

object Iso {
  type Aux[TC[_[_]], ADT2[_], Ev2[_[_]]] = Iso[TC] { type ADT[X] = ADT2[X] ; type Ev[P[_]] = Ev2[P] }
  type WithEv[TC[_[_]], Ev2[_[_]]] = Iso[TC] { type Ev[P[_]] = Ev2[P] }
}

class FooTest extends FunSpec with Matchers {
  import scalaz.Monad

  @foo trait LensAlg[P[_], A] {
    type Q[_]

    val M: scalaz.Monad[P]

    def get: P[A]
    def set(a: A): P[Unit]

    def modify(f: A => A): P[Unit] = M.bind(get)(f andThen set)
  }
  object LensAlg {
    // type Aux[P[_], A, Q2[_]] = LensAlg[P, A] { type Q[X] = Q2[X] }
  }

  type Region = Long
  type DID = Long

  @foo trait Geofence[P[_]] {

    /* OPTICS */
    /* @optic */ val regionLn: LensAlg[P, Region]
    /* @optic */ val insideLn: LensAlg[P, Set[DID]] // TODO(jfuentes): protected PUT public GET ?

    /* DERIVED */
    @derived def addInside(did: DID): P[Unit] = insideLn.modify(_ + did)
    @derived def removeInside(did: DID): P[Unit] = insideLn.modify(_ - did)

  }

  /* EXAMPLES */

  Geofence.RegionLn(LensAlg.Get())
  Geofence.RegionLn(LensAlg.Set(5L))
  Geofence.AddInside(4)
  Geofence.RemoveInside(10)

}

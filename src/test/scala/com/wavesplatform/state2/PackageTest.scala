package com.wavesplatform.state2

import cats.data.{NonEmptyList => NEL}
import cats.kernel.Monoid
import org.scalatest.{FunSuite, Matchers}

class PackageTest extends FunSuite with Matchers {
  test("prependCompact") {

    case class Compactable(i: Int)
    implicit val compactableMonoid: Monoid[Compactable] = new Monoid[Compactable] {
      override def empty = Compactable(0)

      override def combine(x: Compactable, y: Compactable) = Compactable(x.i + y.i)
    }

    val r = prependCompact(Compactable(8), NEL.of(Compactable(1), Compactable(1), Compactable(1))) { case (x, y) => x.i + y.i <= 10 }
    r shouldBe NEL.of(Compactable(9), Compactable(1), Compactable(1))
  }

  test("splitAfterThreshold") {
    splitAfterThreshold(NEL.of(1, 2, 3, 4, 5, 6), 5)(x => x) shouldBe ((NEL.of(1, 2, 3), List(4, 5, 6)))
    splitAfterThreshold(NEL.of(1, 2, 3, 4, 5, 6), 6)(x => x) shouldBe ((NEL.of(1, 2, 3), List(4, 5, 6)))
    splitAfterThreshold(NEL.of(1, 2, 3, 4, 5, 6), 7)(x => x) shouldBe ((NEL.of(1, 2, 3, 4), List(5, 6)))
  }
}

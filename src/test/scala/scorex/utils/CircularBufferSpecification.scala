package scorex.utils

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class CircularBufferSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  def filledCB(maxCount: Int): CircularBuffer[Int] = {
    val cb = new CircularBuffer[Int](maxCount)
    (1 to maxCount).foreach {
      cb += _
    }
    cb
  }

  property("CircularBuffer shouldn't extend more than max count of element") {
    val maxCount = 10
    val cb = filledCB(maxCount)
    cb += maxCount + 1
    cb.size shouldBe maxCount
  }

  property("CircularBuffer should add elements in circle") {
    val maxCount = 10
    val cb = filledCB(maxCount)
    cb += maxCount + 1
    cb += maxCount + 2
    cb.size shouldBe maxCount
    cb(0) shouldBe maxCount + 1
    cb(1) shouldBe maxCount + 2
  }

  property("CircularBuffer should remove elements in circle") {
    val maxCount = 10
    val cb = filledCB(maxCount)
    cb += maxCount + 1
    cb.remove(2)
    cb += maxCount + 2
    cb(maxCount - 1) shouldBe maxCount + 2
  }

  property("CircularBuffer should add to the end after remove an element") {
    val maxCount = 5
    val cb = filledCB(maxCount)
    cb += 6
    cb += 7
    cb.remove(3)
    cb += 8
    cb.buffer shouldBe Seq(6, 7, 4, 5, 8)
    cb += 9
    cb.buffer shouldBe Seq(9, 7, 4, 5, 8)
    cb.remove(4) shouldBe Some(4)
  }

  property("CircularBuffer should drop from middle ") {
    val cb = filledCB(5)
    cb.drop(Seq(3, 4).contains(_))
    cb.buffer shouldBe Seq(1, 2, 5)

    cb += 6
    cb += 7
    cb.buffer shouldBe Seq(1, 2, 5, 6, 7)

    cb += 8
    cb.buffer shouldBe Seq(8, 2, 5, 6, 7)

    cb.drop(_ == 2) shouldBe Seq(2)
    cb += 9
    cb.buffer shouldBe Seq(8, 5, 6, 7, 9)
  }
}

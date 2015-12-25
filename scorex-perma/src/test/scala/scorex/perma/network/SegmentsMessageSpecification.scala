package scorex.perma.network

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.perma.settings.Constants
import scorex.utils._

class SegmentsMessageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {


  val segmentsMessageSp = SegmentsMessageSpec
  val getSegmentsMessageSpec = GetSegmentsMessageSpec

  property("SegmentsMessageSpec: Encode to bytes round-trip") {
    forAll { (indexes: Seq[Long]) =>
      val ab = AuthDataBlock(randomBytes(Constants.segmentSize), Seq(randomBytes(32),randomBytes(32)))
      val data = indexes.map(i => i -> ab).toMap
      val serialized = segmentsMessageSp.serializeData(data)
      val deserealized = segmentsMessageSp.deserializeData(serialized).get
      indexes.foreach { i =>
        data(i) shouldBe deserealized(i)
      }
    }
  }

  property("GetSegmentsMessageSpec: Encode to bytes round-trip") {
    forAll { (indexes: Seq[Long]) =>
      val serialized = getSegmentsMessageSpec.serializeData(indexes)
      val deserealized = getSegmentsMessageSpec.deserializeData(serialized).get
      indexes shouldBe deserealized
    }
  }

}
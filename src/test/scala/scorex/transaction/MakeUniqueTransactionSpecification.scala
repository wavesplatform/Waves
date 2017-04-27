package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.MakeAssetNameUniqueTransaction

class MakeUniqueTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Make unique serialization roundtrip") {
    forAll(makeAssetNameUniqueGen) { makeUnique: MakeAssetNameUniqueTransaction =>
      val recovered = MakeAssetNameUniqueTransaction.parseBytes(makeUnique.bytes).get
      recovered.bytes shouldEqual makeUnique.bytes
    }
  }

  property("Make unique serialization from TypedTransaction") {
    forAll(makeAssetNameUniqueGen) { makeUnique: MakeAssetNameUniqueTransaction =>
      val recovered = TransactionParser.parseBytes(makeUnique.bytes).get
      recovered.bytes shouldEqual makeUnique.bytes
    }
  }

}

package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.MakeAssetNameUniqueTransaction

class MakeAssetNameUniqueTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Make asset name unique serialization roundtrip") {
    forAll(makeAssetNameUniqueGen) { makeAssetNameUnique: MakeAssetNameUniqueTransaction =>
      val recovered = MakeAssetNameUniqueTransaction.parseBytes(makeAssetNameUnique.bytes).get
      recovered.bytes shouldEqual makeAssetNameUnique.bytes
    }
  }

  property("Make asset name unique serialization from TypedTransaction") {
    forAll(makeAssetNameUniqueGen) { makeAssetNameUnique: MakeAssetNameUniqueTransaction =>
      val recovered = TransactionParser.parseBytes(makeAssetNameUnique.bytes).get
      recovered.bytes shouldEqual makeAssetNameUnique.bytes
    }
  }

}

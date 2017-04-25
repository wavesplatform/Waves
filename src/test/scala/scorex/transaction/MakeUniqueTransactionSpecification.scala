package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.assets.MakeUniqueAssetTransaction

class MakeUniqueTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Make unique serialization roundtrip") {
    forAll(makeUniqueGen) { makeUnique: MakeUniqueAssetTransaction =>
      val recovered = MakeUniqueAssetTransaction.parseBytes(makeUnique.bytes).get
      recovered.bytes shouldEqual makeUnique.bytes
    }
  }

  property("Make unique serialization from TypedTransaction") {
    forAll(makeUniqueGen) { makeUnique: MakeUniqueAssetTransaction =>
      val recovered = TransactionParser.parseBytes(makeUnique.bytes).get
      recovered.bytes shouldEqual makeUnique.bytes
    }
  }

}

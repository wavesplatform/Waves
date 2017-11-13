package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.ByteStr
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.TransactionParser

class MicroBlockInvMessageSpecSpec extends FreeSpec
  with Matchers
  with PropertyChecks
  with Eventually
  with TransactionGen {

  private val microBlockInvGen: Gen[MicroBlockInv] = for {
    acc <- accountGen
    totalSig <- byteArrayGen(TransactionParser.SignatureLength)
    prevBlockSig <- byteArrayGen(TransactionParser.SignatureLength)
  } yield MicroBlockInv(acc, ByteStr(totalSig), ByteStr(prevBlockSig))

  "MicroBlockInvMessageSpec" - {
    import MicroBlockInvMessageSpec._

    "deserializeData(serializedData(data)) == data" in forAll(microBlockInvGen) { inv =>
      inv.signaturesValid()shouldBe 'right
      val restoredInv = deserializeData(serializeData(inv)).get
      restoredInv.signaturesValid() shouldBe 'right

      restoredInv shouldBe inv
    }
  }

}

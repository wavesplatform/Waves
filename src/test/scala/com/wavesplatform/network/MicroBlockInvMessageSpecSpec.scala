package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.ByteStr
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.{Signed, TransactionParser}

class MicroBlockInvMessageSpecSpec extends FreeSpec
  with Matchers
  with PropertyChecks
  with Eventually
  with GeneratorDrivenPropertyChecks
  with TransactionGen {

  private val microBlockInvGen: Gen[MicroBlockInv] = for {
    acc <- accountGen
    totalSig <- byteArrayGen(TransactionParser.SignatureLength)
    prevBlockSig <- byteArrayGen(TransactionParser.SignatureLength)
  } yield MicroBlockInv(acc, ByteStr(totalSig), ByteStr(prevBlockSig))

  "MicroBlockInvMessageSpec" - {
    import MicroBlockInvMessageSpec._

    "deserializeData(serializedData(data)) == data" in forAll(microBlockInvGen) { inv =>
      Signed.validateSignatures(inv) shouldBe 'right
      val restoredInv = deserializeData(serializeData(inv)).get
      Signed.validateSignatures(restoredInv) shouldBe 'right

      restoredInv shouldBe inv
    }
  }

}

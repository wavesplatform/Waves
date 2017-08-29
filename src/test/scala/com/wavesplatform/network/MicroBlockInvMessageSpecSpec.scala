package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.ByteStr
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.TransactionParser

class MicroBlockInvMessageSpecSpec extends FreeSpec
  with Matchers
  with PropertyChecks
  with Eventually
  with GeneratorDrivenPropertyChecks
  with TransactionGen {

  private val microBlockInvGen: Gen[MicroBlockInv] = for {
    totalSig <- byteArrayGen(TransactionParser.SignatureLength)
    prevBlockSig <- byteArrayGen(TransactionParser.SignatureLength)
    created <- timestampGen
  } yield MicroBlockInv(ByteStr(totalSig), ByteStr(prevBlockSig), created)

  "MicroBlockInvMessageSpec" - {
    import MicroBlockInvMessageSpec._

    "deserializeData(serializedData(data)) == data" in forAll(microBlockInvGen) { microBlock =>
      deserializeData(serializeData(microBlock)).toOption should contain(microBlock)
    }
  }

}

package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto._
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class MicroBlockInvSpecSpec extends FreeSpec with Matchers with PropertyChecks with Eventually with TransactionGen {

  private val microBlockInvGen: Gen[MicroBlockInv] = for {
    acc          <- accountGen
    totalSig     <- byteArrayGen(SignatureLength)
    prevBlockSig <- byteArrayGen(SignatureLength)
  } yield MicroBlockInv(acc, ByteStr(totalSig), ByteStr(prevBlockSig))

  "MicroBlockInvMessageSpec" - {
    import MicroBlockInvSpec._

    "deserializeData(serializedData(data)) == data" in forAll(microBlockInvGen) { inv =>
      inv.signaturesValid() shouldBe 'right
      val restoredInv = deserializeData(serializeData(inv)).get
      restoredInv.signaturesValid() shouldBe 'right

      restoredInv shouldBe inv
    }
  }

}

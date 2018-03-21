package com.wavesplatform.state2.diffs.smart.predef

import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scodec.bits.ByteVector
import scorex.account.Address

class AddressTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  property("should calculate correct address from public key") {
    forAll(accountGen) { acc =>
      val script =
        s"""
           | let pk = base58'${ByteStr(acc.publicKey).base58}'
           | let address = addressFromPublicKey(pk)
           | address.bytes
        """.stripMargin
      runScript[ByteVector](script) shouldBe Right(ByteVector(Address.fromPublicKey(acc.publicKey, networkByte).bytes.arr))
    }
  }
}

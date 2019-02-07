package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.state._
import com.wavesplatform.lang.Testing._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scodec.bits.ByteVector
import com.wavesplatform.account.Address

class AddressTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  property("should calculate address from public key") {
    forAll(accountGen) { acc =>
      val script =
        s"""
           | let pk = base58'${ByteStr(acc.publicKey).base58}'
           | let address = addressFromPublicKey(pk)
           | address.bytes
        """.stripMargin
      runScript(script) shouldBe evaluated(ByteVector(Address.fromPublicKey(acc.publicKey, chainId).bytes.arr))
    }
  }

  property("should calculate address from bytes") {
    forAll(accountGen) { acc =>
      val addressBytes = Address.fromPublicKey(acc.publicKey, chainId).bytes
      val script =
        s"""
           | let addressString = "${addressBytes.base58}"
           | let maybeAddress = addressFromString(addressString)
           | let address = extract(maybeAddress)
           | address.bytes
        """.stripMargin
      runScript(script) shouldBe evaluated(ByteVector(Address.fromBytes(addressBytes.arr, chainId).explicitGet().bytes.arr))
    }
  }

  property("should calculate address and return bytes without intermediate ref") {
    forAll(accountGen) { acc =>
      val addressBytes = Address.fromPublicKey(acc.publicKey, chainId).bytes
      val script =
        s"""
           | let addressString = "${addressBytes.base58}"
           | let maybeAddress = addressFromString(addressString)
           | extract(maybeAddress).bytes
        """.stripMargin
      runScript(script) shouldBe evaluated(ByteVector(Address.fromBytes(addressBytes.arr, chainId).explicitGet().bytes.arr))
    }
  }
}

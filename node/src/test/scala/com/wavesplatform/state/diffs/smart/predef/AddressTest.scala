package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Testing._
import com.wavesplatform.state.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AddressTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  property("should calculate address from public key") {
    forAll(accountGen) { acc =>
      val script =
        s"""
           | let pk = base58'${acc.publicKey}'
           | let address = addressFromPublicKey(pk)
           | address.bytes
        """.stripMargin
      runScript(script) shouldBe evaluated(ByteStr(Address.fromPublicKey(acc.publicKey, chainId).bytes))
    }
  }

  property("should calculate address from bytes") {
    forAll(accountGen) { acc =>
      val address = Address.fromPublicKey(acc.publicKey, chainId)
      val script =
        s"""
           | let addressString = "$address"
           | let maybeAddress = addressFromString(addressString)
           | let address = extract(maybeAddress)
           | address.bytes
        """.stripMargin
      runScript(script) shouldBe evaluated(ByteStr(Address.fromBytes(address.bytes, chainId).explicitGet().bytes))
    }
  }

  property("should calculate address and return bytes without intermediate ref") {
    forAll(accountGen) { acc =>
      val address = Address.fromPublicKey(acc.publicKey, chainId)
      val script =
        s"""
           | let addressString = "$address"
           | let maybeAddress = addressFromString(addressString)
           | extract(maybeAddress).bytes
        """.stripMargin
      runScript(script) shouldBe evaluated(ByteStr(Address.fromBytes(address.bytes, chainId).explicitGet().bytes))
    }
  }

  property("should fails on illegal bytes length") {
    val correctLength = Address.AddressLength
    Address.fromBytes(Array.emptyByteArray) should produce ("Wrong addressBytes length")
    Address.fromBytes(Array(1)) should produce ("Wrong addressBytes length")
    Address.fromBytes(Array.fill(correctLength - 1)(1)) should produce ("Wrong addressBytes length")
    Address.fromBytes(Array.fill(correctLength + 1)(1)) should produce ("Wrong addressBytes length")
  }
}

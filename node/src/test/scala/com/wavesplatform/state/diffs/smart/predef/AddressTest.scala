package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Testing._
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4}
import com.wavesplatform.state.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.transaction.TxValidationError.InvalidAddress

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
    forAll(for {
      account <- accountGen
      version <- Gen.oneOf(DirectiveDictionary[StdLibVersion].all)
    } yield (account, version)) { case (account, version) =>
      val address = Address.fromPublicKey(account.publicKey, chainId)
      val script =
        s"""
           | let addressString = "$address"
           | let maybeAddress = addressFromString(addressString)
           | let address = extract(maybeAddress)
           | address.bytes
        """.stripMargin
      runScript(script, ctxV = version) shouldBe evaluated(ByteStr(Address.fromBytes(address.bytes, chainId).explicitGet().bytes))
    }
  }

  property("should calculate address and return bytes without intermediate ref") {
    forAll(for {
      account <- accountGen
      version <- Gen.oneOf(DirectiveDictionary[StdLibVersion].all)
    } yield (account, version)) { case (account, version) =>
      val address = Address.fromPublicKey(account.publicKey, chainId)
      val script =
        s"""
           | let addressString = "$address"
           | let maybeAddress = addressFromString(addressString)
           | extract(maybeAddress).bytes
        """.stripMargin
      runScript(script, ctxV = version) shouldBe evaluated(ByteStr(Address.fromBytes(address.bytes, chainId).explicitGet().bytes))
    }
  }

  property("should fails on illegal bytes length") {
    val correctLength = Address.AddressLength
    Address.fromBytes(Array.emptyByteArray) should produce("Wrong addressBytes length")
    Address.fromBytes(Array(1)) should produce("Wrong addressBytes length")
    Address.fromBytes(Array.fill(correctLength - 1)(1)) should produce("Wrong addressBytes length")
    Address.fromBytes(Array.fill(correctLength + 1)(1)) should produce("Wrong addressBytes length")
  }

  property("RIDE addressFromString V4 limit exceeding result") {
    runScript(s""" addressFromString("${"a" * 37}") """, ctxV = V4) shouldBe Right(unit)
  }

  property("Address.fromString errors") {
    Address.fromString("a" * 37) shouldBe Left(InvalidAddress("Wrong address string length: max=36, actual: 37"))
    Address.fromString("a" * 36) shouldBe Left(InvalidAddress("Wrong addressBytes length: expected: 26, actual: 27"))
    Address.fromString("a" * 35) shouldBe Left(InvalidAddress("Unknown address version: 18"))
  }
}

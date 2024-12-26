package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Testing.*
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V4}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CaseObj}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.state.diffs.smart.predef
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxValidationError.InvalidAddress

class AddressTest extends PropSpec {
  property("should calculate address from public key") {
    val acc = TxHelpers.signer(1)

    val script =
      s"""
         | let pk = base58'${acc.publicKey}'
         | let address = addressFromPublicKey(pk)
         | address.bytes
      """.stripMargin
    runScript(script) shouldBe evaluated(ByteStr(Address.fromPublicKey(acc.publicKey, chainId).bytes))
  }

  property("should calculate address from bytes") {
    val account = TxHelpers.signer(1)

    DirectiveDictionary[StdLibVersion].all.foreach { version =>
      val extractFunction = if (version >= V4) "value" else "extract"
      val address         = Address.fromPublicKey(account.publicKey, predef.chainId)
      val script =
        s"""
           | let addressString = "$address"
           | let maybeAddress = addressFromString(addressString)
           | let address = $extractFunction(maybeAddress)
           | address.bytes
        """.stripMargin
      runScript(script, ctxV = version, chainId = predef.chainId) shouldBe evaluated(ByteStr(address.bytes))
    }
  }

  property("should calculate address and return bytes without intermediate ref") {
    val account = TxHelpers.signer(1)

    DirectiveDictionary[StdLibVersion].all.foreach { version =>
      val extractFunction = if (version >= V4) "value" else "extract"
      val address         = Address.fromPublicKey(account.publicKey, predef.chainId)
      val script =
        s"""
           | let addressString = "$address"
           | let maybeAddress = addressFromString(addressString)
           | $extractFunction(maybeAddress).bytes
        """.stripMargin
      runScript(script, ctxV = version, chainId = predef.chainId) shouldBe evaluated(ByteStr(address.bytes))
    }
  }

  property("should fails on illegal bytes length") {
    val correctLength = Address.AddressLength
    Address.fromBytes(Array.emptyByteArray) should produce("Wrong addressBytes length")
    Address.fromBytes(Array(1)) should produce("Wrong addressBytes length")
    Address.fromBytes(Array.fill(correctLength - 1)(1)) should produce("Wrong addressBytes length")
    Address.fromBytes(Array.fill(correctLength + 1)(1)) should produce("Wrong addressBytes length")
  }

  property("Address.fromString errors") {
    Address.fromString("a" * 37) shouldBe Left(InvalidAddress("Wrong address string length: max=36, actual: 37"))
    Address.fromString("a" * 36) shouldBe Left(InvalidAddress("Wrong addressBytes length: expected: 26, actual: 27"))
    Address.fromString("a" * 35) shouldBe Left(InvalidAddress("Unknown address version: 18"))
  }

  property("RIDE addressFromString V4 limit exceeding result") {
    runScript(s""" addressFromString("${"a" * 37}") """, ctxV = V4) shouldBe Right(unit)
    runScript(s""" addressFromString("${"a" * 36}") """, ctxV = V4) shouldBe Right(unit)
    runScript(s""" addressFromString("${"a" * 35}") """, ctxV = V4) shouldBe Right(unit)
  }

  property("RIDE addressFromString V4 success") {
    val base58 = """3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU"""
    val result = runScript(s""" addressFromString("$base58") """, ctxV = V4, chainId = 'T')
      .explicitGet()
      .asInstanceOf[CaseObj]
    result.caseType.name shouldBe "Address"
    result.fields shouldBe Map("bytes" -> CONST_BYTESTR(ByteStr.decodeBase58(base58).get).explicitGet())
  }
}

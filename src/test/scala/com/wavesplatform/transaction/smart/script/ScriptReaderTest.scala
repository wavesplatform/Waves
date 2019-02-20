package com.wavesplatform.transaction.smart.script

import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.{NoShrink, TransactionGen, crypto}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Inside, Matchers, PropSpec}

class ScriptReaderTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with Inside with NoShrink {
  val checksumLength = 4

  property("should parse all bytes for V1") {
    forAll(exprGen) { sc =>
      val body     = Array(V1.toByte) ++ Serde.serialize(sc) ++ "foo".getBytes
      val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
      ScriptReader.fromBytes(allBytes) should produce("bytes left")
    }
  }

  property("should parse all bytes for V3") {
    forAll(contractGen) { sc =>
      val allBytes = ContractScript.apply(V3, sc).explicitGet().bytes().arr
      ScriptReader.fromBytes(allBytes).explicitGet().expr shouldBe sc
    }
  }
}

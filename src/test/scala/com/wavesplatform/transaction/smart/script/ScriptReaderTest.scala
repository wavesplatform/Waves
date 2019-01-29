package com.wavesplatform.transaction.smart.script

import com.wavesplatform.crypto
import com.wavesplatform.lang.Version.{ExprV1, ContractV}
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.transaction.smart.script.v1.ContractScript
import org.scalatest.{FreeSpec, Matchers}

class ScriptReaderTest extends FreeSpec with Matchers {
  val checksumLength = 4

  "should parse all bytes for V1" in {
    val body     = Array(ExprV1.toByte) ++ Serde.serialize(TRUE) ++ "foo".getBytes
    val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
    ScriptReader.fromBytes(allBytes) should produce("bytes left")
  }

  "should parse all bytes for V3" in {
    val sc =
      ContractScript(ContractV,
                     Contract(List.empty, List(ContractFunction(CallableAnnotation("sender"), Terms.FUNC("foo", List("a"), Terms.REF("a")))), None))

    val allBytes = sc.bytes().arr
    ScriptReader.fromBytes(allBytes) shouldBe Right(sc)
  }

}

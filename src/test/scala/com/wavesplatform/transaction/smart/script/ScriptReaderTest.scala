package com.wavesplatform.transaction.smart.script

import com.wavesplatform.common.utils._
import com.wavesplatform.lang.StdLibVersion
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.{NoShrink, crypto}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import org.scalatest.{Inside, Matchers, PropSpec}

class ScriptReaderTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen with Inside with NoShrink {
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

  property("should parse expression with all supported std lib version") {
    val scriptEthList =
      StdLibVersion.SupportedVersions.map { version =>
        ScriptCompiler.compile(s"""
                                  |{-# STDLIB_VERSION ${version} #-}
                                  |  true
                                  """.stripMargin)
      }
    scriptEthList.foreach(_ shouldBe 'right)

    scriptEthList.foreach { scriptEth =>
      ScriptReader.fromBytes(scriptEth.explicitGet()._1.bytes()) shouldBe 'right
    }
  }
}

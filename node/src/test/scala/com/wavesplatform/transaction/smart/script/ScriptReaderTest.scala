package com.wavesplatform.transaction.smart.script

import com.wavesplatform.common.utils._
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.{ContractScript, ScriptReader}
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.{NoShrink, crypto}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import org.scalatest.{Inside, Matchers, PropSpec}

class ScriptReaderTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen with Inside with NoShrink {
  val checksumLength = 4

  property("should parse all bytes for V1") {
    forAll(exprGen) { sc =>
      val body     = Array(V1.id.toByte) ++ Serde.serialize(sc) ++ "foo".getBytes("UTF-8")
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
      DirectiveDictionary[StdLibVersion].all.map { version =>
        ScriptCompiler.compile(s"""
                                  |{-# STDLIB_VERSION ${version.value} #-}
                                  |  true
                                  """.stripMargin)
      }
    scriptEthList.foreach(_ shouldBe 'right)

    scriptEthList.foreach { scriptEth =>
      ScriptReader.fromBytes(scriptEth.explicitGet()._1.bytes()) shouldBe 'right
    }
  }

  property("should return correct error for invalid starting bytes") {
    import ScriptReaderTest._

    forAll(Gen.oneOf(invalidPrefix, invalidPrefixV0)) { scBytes =>
      ScriptReader.fromBytes(scBytes) shouldBe 'left
    }
  }
}

object ScriptReaderTest {

  val validStdLibVersions: Set[Int] = DirectiveDictionary[StdLibVersion].all.map(_.id).toSet

  // version byte 0 but no StdLibVersion byte and/or ContentType byte
  val invalidPrefixV0: Gen[Array[Byte]] =
    for {
      n  <- Gen.oneOf(0, 1)
      bs <- Gen.listOfN(n, Arbitrary.arbitrary[Byte])
    } yield 0.toByte +: bs.toArray

  // invalid version byte and unknown length of remaining bytes
  val invalidPrefix: Gen[Array[Byte]] =
    for {
      v   <- Arbitrary.arbitrary[Byte].filter(b => !validStdLibVersions.contains(b) && b != 0)
      bs  <- Gen.listOf(Arbitrary.arbitrary[Byte])
    } yield v +: bs.toArray
}
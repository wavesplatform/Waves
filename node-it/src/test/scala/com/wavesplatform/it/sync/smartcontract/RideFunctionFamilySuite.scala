package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.ScriptCompilerError
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{CompiledScript, DecompiledScript, EstimatedScript}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import org.scalatest.CancelAfterFailure

class RideFunctionFamilySuite extends BaseTransactionSuite with CancelAfterFailure {

  def ffAssetScript(version: Int): String =
    s"""
       |{-# STDLIB_VERSION $version #-}
       |{-# CONTENT_TYPE EXPRESSION #-}
       |{-# SCRIPT_TYPE ASSET #-}
       |  
       |let a = base58''
       | 
       |sigVerify_8Kb(a, a, a) && sigVerify_16Kb(a, a, a) && rsaVerify_32Kb(SHA3512, a, a, a) && a == (blake2b256_64Kb(a) + keccak256_128Kb(a) + sha256(a))
       |""".stripMargin

  def ffDApp(version: Int)(f: String): String =
    s"""
       |{-# STDLIB_VERSION $version #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |  
       |let a = base58''
       | 
       |func binary(value: ByteVector) = [ BinaryEntry("binary", value) ]
       |func boolean(value: Boolean) = [ BooleanEntry("boolean", value) ]
       |""".stripMargin ++ Map(
      "blake" -> """
     |@Callable(inv)
     |func blake() = binary(blake2b256(a) + blake2b256_16Kb(a) + blake2b256_32Kb(a) + blake2b256_64Kb(a) + blake2b256_128Kb(a))
     |""",
      "keccak" -> """
     |@Callable(inv)
     |func keccak() = binary(keccak256(a) + keccak256_16Kb(a) + keccak256_32Kb(a) + keccak256_64Kb(a) + keccak256_128Kb(a))
     |""",
      "sha" -> """
     |@Callable(inv)
     |func sha() = binary(sha256(a) + sha256_16Kb(a) + sha256_32Kb(a) + sha256_64Kb(a) + sha256_128Kb(a))
     |""",
      "sig" -> """
     |@Callable(inv)
     |func sig() = boolean(sigVerify(a, a, a) && sigVerify_8Kb(a, a, a) && sigVerify_16Kb(a, a, a) && sigVerify_32Kb(a, a, a) && sigVerify_64Kb(a, a, a) && sigVerify_128Kb(a, a, a))
     |""",
      "rsa1" -> """
     |@Verifier(tx)
     |func rsa() = rsaVerify(NOALG, a, a, a) && rsaVerify_16Kb(MD5, a, a, a)
     |""",
      "rsa2" -> """
     |@Verifier(tx)
     |func rsa() = rsaVerify_32Kb(SHA256, a, a, a) && rsaVerify_64Kb(SHA3256, a, a, a) && rsaVerify_128Kb(NOALG, a, a, a)
     |"""
    )(f).stripMargin

  test("function family in asset script") {
    val CompiledScript(scr, complexity, _)    = sender.scriptCompile(ffAssetScript(4))
    val EstimatedScript(_, _, ecomplexity, _) = sender.scriptEstimate(scr)
    ecomplexity shouldBe complexity
    ecomplexity shouldBe 1027

    val DecompiledScript(dec) = sender.scriptDecompile(scr)
    List("sigVerify_16Kb(a, a, a)", "rsaVerify_32Kb(SHA3512, a, a, a)", "blake2b256_64Kb(a)", "keccak256_128Kb(a)", "sha256(a)").forall(
      dec.contains
    ) shouldBe true
    dec.contains("Native") shouldBe false
  }

  test("function family in asset script V3") {
    assertApiError(sender.scriptCompile(ffAssetScript(3))) { error =>
      error.statusCode shouldBe 400
      error.id shouldBe ScriptCompilerError.Id
      error.message should include("Can't find a function")
    }
  }

  test("function family (hashes)") {
    for (
      (hash, names) <- List(
        "blake"  -> List("blake2b256(a)", "blake2b256_16Kb(a)", "blake2b256_32Kb(a)", "blake2b256_64Kb(a)", "blake2b256_128Kb(a)"),
        "keccak" -> List("keccak256(a)", "keccak256_16Kb(a)", "keccak256_32Kb(a)", "keccak256_64Kb(a)", "keccak256_128Kb(a)"),
        "sha"    -> List("sha256(a)", "sha256_16Kb(a)", "sha256_32Kb(a)", "sha256_64Kb(a)", "sha256_128Kb(a)")
      )
    ) {
      val CompiledScript(scr, complexity, _)    = sender.scriptCompile(ffDApp(4)(hash))
      val EstimatedScript(_, _, ecomplexity, _) = sender.scriptEstimate(scr)
      ecomplexity shouldBe complexity
      ecomplexity shouldBe 405
      val DecompiledScript(dec) = sender.scriptDecompile(scr)
      names.forall(dec.contains) shouldBe true
      dec.contains("Native") shouldBe false
    }
  }

  test("function family (verify)") {
    for (
      ((f, names), c) <- List(
        "sig" -> List(
          "sigVerify(a, a, a)",
          "sigVerify_8Kb(a, a, a)",
          "sigVerify_16Kb(a, a, a)",
          "sigVerify_32Kb(a, a, a)",
          "sigVerify_64Kb(a, a, a)",
          "sigVerify_128Kb(a, a, a)"
        )      -> 678,
        "rsa1" -> List("rsaVerify(NOALG, a, a, a)", "rsaVerify_16Kb(MD5, a, a, a)")                                              -> 1510,
        "rsa2" -> List("rsaVerify_32Kb(SHA256, a, a, a)", "rsaVerify_64Kb(SHA3256, a, a, a)", "rsaVerify_128Kb(NOALG, a, a, a)") -> 1940
      )
    ) {
      val CompiledScript(scr, complexity, _)    = sender.scriptCompile(ffDApp(4)(f))
      val EstimatedScript(_, _, ecomplexity, _) = sender.scriptEstimate(scr)
      ecomplexity shouldBe complexity
      ecomplexity shouldBe c
      val DecompiledScript(dec) = sender.scriptDecompile(scr)
      names.forall(dec.contains) shouldBe true
      dec.contains("Native") shouldBe false
    }
  }

  test("function family (DAps in V3)") {
    for (f <- List("blake", "keccak", "sha", "sig", "rsa1", "rsa2")) {
      assertApiError(sender.scriptCompile(ffDApp(3)(f))) { error =>
        error.statusCode shouldBe 400
        error.id shouldBe ScriptCompilerError.Id
        error.message should include("Can't find a function")
      }
    }
  }

}

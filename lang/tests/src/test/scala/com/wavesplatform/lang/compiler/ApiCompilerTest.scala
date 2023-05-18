package com.wavesplatform.lang.compiler

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.API
import com.wavesplatform.lang.CompileAndParseResult.{Contract, Expression}
import com.wavesplatform.lang.v1.compiler.CompilationError
import com.wavesplatform.lang.v1.compiler.CompilationError.{DefNotFound, Generic}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.test.PropSpec
import org.scalatest.Assertion

class ApiCompilerTest extends PropSpec {

  property("compile should return correct error positions in script with libraries") {

    // length of lib code < lib import statement
    checkCompile(dAppWithParseError, smallLib, "Parse error: illegal expression in 104-107")
    checkCompile(dAppWithCompileError, smallLib, "Compilation failed: [A definition of 'abc' is not found in 140-143]")
    checkCompile(verifierWithParseError, smallLib, "Parse error: expected variable name in 110-113")
    checkCompile(verifierWithCompileError, smallLib, "Compilation failed: [A definition of 'abc' is not found in 110-113]")

    // length of lib code > lib import statement
    checkCompile(dAppWithParseError, bigLib, "Parse error: illegal expression in 104-107")
    checkCompile(dAppWithCompileError, bigLib, "Compilation failed: [A definition of 'abc' is not found in 140-143]")
    checkCompile(verifierWithParseError, bigLib, "Parse error: expected variable name in 110-113")
    checkCompile(verifierWithCompileError, bigLib, "Compilation failed: [A definition of 'abc' is not found in 110-113]")

    // errors with positions of library code should have resulting position of import statement
    checkCompile(correctDapp, libWithParseError1, "Parse error: illegal expression in 79-102")
    checkCompile(correctDapp, libWithCompileError, "Compilation failed: [A definition of 'abc' is not found in 79-102]")
    checkCompile(correctVerifier, libWithParseError2, "Parse error: expected variable name in 85-108")
    checkCompile(correctVerifier, libWithCompileError, "Compilation failed: [A definition of 'abc' is not found in 85-108]")
  }

  property("parseAndCompile should return correct error positions in script with libraries") {
    // length of lib code < lib import statement
    checkParseAndCompileDapp(
      dAppWithParseError,
      smallLib,
      Seq(Generic(0, 109, "Parsing failed. Some chars was removed as result of recovery process."))
    )
    checkParseAndCompileDapp(dAppWithCompileError, smallLib, Seq(DefNotFound(140, 143, "abc")))
    checkParseAndCompileVerifier(
      verifierWithParseError,
      smallLib,
      Seq(
        DefNotFound(110, 112, "le"),
        Generic(0, 0, "Script should return boolean"),
        Generic(112, 113, "Parsing failed. Some chars was removed as result of recovery process.")
      )
    )
    checkParseAndCompileVerifier(verifierWithCompileError, smallLib, Seq(DefNotFound(110, 113, "abc")))

    // length of lib code > lib import statement
    checkParseAndCompileDapp(
      dAppWithParseError,
      bigLib,
      Seq(Generic(0, 109, "Parsing failed. Some chars was removed as result of recovery process."))
    )
    checkParseAndCompileDapp(dAppWithCompileError, bigLib, Seq(DefNotFound(140, 143, "abc")))
    checkParseAndCompileVerifier(
      verifierWithParseError,
      bigLib,
      Seq(
        DefNotFound(110, 112, "le"),
        Generic(0, 0, "Script should return boolean"),
        Generic(112, 113, "Parsing failed. Some chars was removed as result of recovery process.")
      )
    )
    checkParseAndCompileVerifier(verifierWithCompileError, bigLib, Seq(DefNotFound(110, 113, "abc")))

    // errors with positions of library code should have resulting position of import statement
    checkParseAndCompileDapp(
      correctDapp,
      libWithParseError1,
      Seq(Generic(0, 102, "Parsing failed. Some chars was removed as result of recovery process."))
    )
    checkParseAndCompileDapp(correctDapp, libWithCompileError, Seq(DefNotFound(79, 102, "abc")))
    checkParseAndCompileVerifier(
      correctVerifier,
      libWithParseError2,
      Seq(Generic(85, 110, "Parsing failed. Some chars was removed as result of recovery process."))
    )
    checkParseAndCompileVerifier(correctVerifier, libWithCompileError, Seq(DefNotFound(85, 108, "abc")))
  }

  private def checkCompile(script: String, lib: String, expectedError: String): Assertion = {
    val estimator: ScriptEstimator = ScriptEstimator.all(true).last
    API.compile(script, estimator, libraries = Map(libName -> lib)) shouldBe Left(expectedError)
  }

  private def checkParseAndCompileDapp(dApp: String, lib: String, expectedErrors: Seq[CompilationError]): Assertion =
    API.parseAndCompile(dApp, libraries = Map(libName -> lib)).explicitGet().asInstanceOf[Contract].errors shouldBe expectedErrors

  private def checkParseAndCompileVerifier(verifier: String, lib: String, expectedErrors: Seq[CompilationError]): Assertion =
    API.parseAndCompile(verifier, libraries = Map(libName -> lib)).explicitGet().asInstanceOf[Expression].errors shouldBe expectedErrors

  private def libName = "lib.ride"
  private def dAppWithParseError: String =
    s"""{-# STDLIB_VERSION 6 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# IMPORT $libName #-}
       |
       |abc
       |
       |@Callable(i)
       |func f() = []
       |""".stripMargin

  private def dAppWithCompileError: String =
    s"""{-# STDLIB_VERSION 6 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# IMPORT $libName #-}
       |
       |@Callable(i)
       |func f() = {
       |  let a = abc + 1
       |  []
       |}
       |""".stripMargin

  private def correctDapp: String =
    s"""{-# STDLIB_VERSION 6 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# IMPORT $libName #-}
       |
       |@Callable(i)
       |func f() = []
       |""".stripMargin

  private def verifierWithParseError: String =
    s"""{-# STDLIB_VERSION 6 #-}
       |{-# CONTENT_TYPE EXPRESSION #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# IMPORT $libName #-}
       |
       |let
       |""".stripMargin

  private def verifierWithCompileError: String =
    s"""{-# STDLIB_VERSION 6 #-}
       |{-# CONTENT_TYPE EXPRESSION #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# IMPORT $libName #-}
       |
       |abc == 1
       |""".stripMargin

  private def correctVerifier: String =
    s"""{-# STDLIB_VERSION 6 #-}
       |{-# CONTENT_TYPE EXPRESSION #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# IMPORT $libName #-}
       |
       |1 == 1
       |""".stripMargin

  private def smallLib: String =
    s"""{-# CONTENT_TYPE LIBRARY #-}
       |func g() = []
       |""".stripMargin

  private def bigLib: String =
    s"""{-# CONTENT_TYPE LIBRARY #-}
       |func g() = {
       | let a = 1
       | let b = 2
       | let c = 3
       | a + b + c
       |}
       |""".stripMargin

  private def libWithParseError1: String =
    s"""{-# CONTENT_TYPE LIBRARY #-}
       |
       |abc
       |""".stripMargin

  private def libWithParseError2: String =
    s"""{-# CONTENT_TYPE LIBRARY #-}
       |
       |let
       |""".stripMargin

  private def libWithCompileError: String =
    s"""{-# CONTENT_TYPE LIBRARY #-}
       |let a = abc + 1
       |""".stripMargin
}

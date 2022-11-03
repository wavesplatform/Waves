package com.wavesplatform

import com.wavesplatform.lang.directives.values.{StdLibVersion, V6}
import utest.{TestSuite, assert}

import scala.scalajs.js.{Dictionary, isUndefined}

abstract class JsTestBase extends TestSuite {
  protected def assertCompileError(code: String, expectingError: String, estimator: Int = 3): Unit = {
    val error = JsAPI.compile(code, estimator).error
    assert(error.toString.contains(expectingError))
  }

  protected def assertCompileSuccess(code: String, estimator: Int = 3): Unit = {
    val error = JsAPI.compile(code, estimator).error
    assert(isUndefined(error))
  }

  protected def expressionComplexity(code: String, version: StdLibVersion = V6, estimator: Int = 3): Int =
    JsAPI.compile(expression(code, version), estimator).complexity.asInstanceOf[Int]

  protected def dAppComplexities(code: String, version: StdLibVersion = V6, estimator: Int = 3): DAppComplexities = {
    val result = JsAPI.compile(dApp(code, version), estimator)
    DAppComplexities(
      result.complexity.asInstanceOf[Int],
      result.verifierComplexity.asInstanceOf[Int],
      result.callableComplexities.asInstanceOf[Dictionary[Int]].toMap,
      result.userFunctionComplexities.asInstanceOf[Dictionary[Int]].toMap,
      result.globalVariableComplexities.asInstanceOf[Dictionary[Int]].toMap
    )
  }

  protected def expression(code: String, version: StdLibVersion): String =
    s"""
       |{-# STDLIB_VERSION ${version.id} #-}
       |{-# CONTENT_TYPE   EXPRESSION    #-}
       |{-# SCRIPT_TYPE    ACCOUNT       #-}
       |
       |$code
     """.stripMargin

  protected def dApp(code: String, version: StdLibVersion): String =
    s"""
       |{-# STDLIB_VERSION ${version.id} #-}
       |{-# CONTENT_TYPE   DAPP          #-}
       |{-# SCRIPT_TYPE    ACCOUNT       #-}
       |
       |$code
     """.stripMargin
}

package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.serialization.SerdeV1

object Lang {

  def compile(input: String): EXPR = compile(input, API.latestEstimatorVersion)

  def compile(input: String, estimatorVersion: Int): EXPR =
    API
      .compile(input, estimatorVersion)
      .fold(
        error => throw new IllegalArgumentException(error),
        {
          case CompileResult.Expression(_, _, expr, _) => expr
          case _                                       => throw new IllegalArgumentException("not an expression")
        }
      )

  def parseAndCompile(input: String, needCompaction: Boolean, removeUnusedCode: Boolean): CompileAndParseResult =
    parseAndCompile(input, API.latestEstimatorVersion, needCompaction, removeUnusedCode)

  def parseAndCompile(input: String, estimatorVersion: Int, needCompaction: Boolean, removeUnusedCode: Boolean): CompileAndParseResult =
    API
      .parseAndCompile(input, estimatorVersion, needCompaction, removeUnusedCode, Map.empty)
      .fold(
        error => throw new IllegalArgumentException(error),
        res => res
      )

  def serializable(expr: Terms.EXPR): Array[Byte] = SerdeV1.serialize(expr)

}

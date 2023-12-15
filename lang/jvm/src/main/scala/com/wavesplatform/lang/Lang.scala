package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.serialization.SerdeV1

import scala.jdk.CollectionConverters.*

object Lang {

  def compile(input: String): EXPR = compile(input, API.latestEstimatorVersion)

  def compile(input: String, estimatorVersion: Int): EXPR =
    (for {
      estimator <- API.estimatorByVersion(estimatorVersion)
      result    <- API.compile(input, estimator)
    } yield result)
      .fold(
        error => throw new IllegalArgumentException(error),
        {
          case CompileResult.Expression(_, _, _, expr, _, _) => expr
          case _                                             => throw new IllegalArgumentException("not an expression")
        }
      )

  def compileDApp(input: String): DAppWithMeta =
    API
      .compile(input, ScriptEstimatorV3.latest)
      .flatMap {
        case r: CompileResult.DApp =>
          val javaMeta = Meta(
            r.meta.argsWithFuncName.view
              .mapValues(_.map { case (argName, argType) => ArgNameWithType(argName, argType.name) }.asJava)
              .toMap
              .asJava
          )
          Right(DAppWithMeta(r.dAppInfo.dApp, javaMeta))
        case _ => Left("not a dApp")
      }
      .fold(e => throw new IllegalArgumentException(e), identity)

  def parseAndCompile(input: String, needCompaction: Boolean, removeUnusedCode: Boolean): CompileAndParseResult =
    parseAndCompile(input, API.latestEstimatorVersion, needCompaction, removeUnusedCode)

  def parseAndCompile(input: String, estimatorVersion: Int, needCompaction: Boolean, removeUnusedCode: Boolean): CompileAndParseResult =
    API
      .parseAndCompile(input, estimatorVersion, needCompaction, removeUnusedCode, Map.empty)
      .fold(
        error => throw new IllegalArgumentException(error),
        res => res
      )

  def serialize(expr: Terms.EXPR): Array[Byte] = SerdeV1.serialize(expr)
}

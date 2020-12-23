package com.wavesplatform.lang

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang
import com.wavesplatform.lang.ScriptMeta.{FunctionArgument, FunctionSignature}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{Directive, DirectiveDictionary, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, ScriptReader}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ContractCompiler, Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment

import scala.jdk.CollectionConverters._

object JavaAdapter {
  private val allDirectives: Iterable[Either[ExecutionError, DirectiveSet]] =
    for {
      version     <- DirectiveDictionary[StdLibVersion].all
      scriptType  <- DirectiveDictionary[ScriptType].all
      contentType <- DirectiveDictionary[ContentType].all
    } yield DirectiveSet(version, scriptType, contentType)

  private val ctxCache: Map[DirectiveSet, CTX[Environment]] =
    allDirectives
      .filter(_.isRight)
      .map(_.explicitGet())
      .map(ds => (ds, buildCtx(ds)))
      .toMap

  private def buildCtx(directiveSet: DirectiveSet): CTX[Environment] =
    CryptoContext.build(Global, directiveSet.stdLibVersion).withEnvironment[Environment] |+|
      WavesContext.build(directiveSet) |+|
      PureContext.build(directiveSet.stdLibVersion).withEnvironment[Environment]

  def compile(input: String): Script = {
    val script =
      for {
        directives   <- DirectiveParser(input)
        directiveSet <- Directive.extractDirectives(directives, defaultStdLib = V4)
        compile = if (directiveSet.contentType == DApp)
          compileDApp _
        else
          compileExpression _
        compiled <- compile(input, ctxCache(directiveSet).compilerContext, directiveSet)
      } yield compiled

    script.fold(error => throw new RideException(error), identity)
  }

  private def compileDApp(
      input: String,
      compilerContext: CompilerContext,
      directiveSet: DirectiveSet
  ): Either[ExecutionError, lang.DApp] =
    for {
      dApp  <- ContractCompiler.compile(input, compilerContext, directiveSet.stdLibVersion)
      bytes <- Global.serializeContract(dApp, directiveSet.stdLibVersion)
    } yield new lang.DApp(
      bytes,
      directiveSet.stdLibVersion.id,
      dApp
    )

  private def compileExpression(
      input: String,
      compilerContext: CompilerContext,
      directiveSet: DirectiveSet
  ): Either[ExecutionError, lang.Expression] =
    for {
      expr  <- ExpressionCompiler.compileBoolean(input, compilerContext)
      bytes <- Right(Global.serializeExpression(expr, directiveSet.stdLibVersion))
    } yield new lang.Expression(
      bytes,
      directiveSet.stdLibVersion.id,
      directiveSet.scriptType == Asset,
      expr
    )

  def parseBytes(bytes: Array[Byte], isAsset: Boolean): Script =
    ScriptReader
      .fromBytes(bytes)
      .map {
        case script: ExprScript =>
          new lang.Expression(bytes, script.stdLibVersion.id, isAsset, script.expr)
        case ContractScript.ContractScriptImpl(stdLibVersion, dApp) =>
          new lang.DApp(bytes, stdLibVersion.id, dApp)
        case _ => ???
      }
      .fold(error => throw new RideException(error.m), identity)

  def decompile(script: Script): String = {
    val version = DirectiveDictionary[StdLibVersion].idMap(script.version())
    script match {
      case dApp: DApp =>
        val ctx = ctxCache(DirectiveSet(version, Account, DApp).explicitGet())
        Decompiler(dApp.internal, ctx.decompilerContext)
      case expression: Expression =>
        val scriptType = if (expression.isAsset) Asset else Account
        val ctx        = ctxCache(DirectiveSet(version, scriptType, Expression).explicitGet())
        Decompiler(expression.internal, ctx.decompilerContext)
      case _ => ???
    }
  }

  def estimate(script: Script): EstimateResult = {
    val version = DirectiveDictionary[StdLibVersion].idMap(script.version())
    val cost = script match {
      case dApp: DApp =>
        ContractScript
          .estimateComplexity(version, dApp.internal, ScriptEstimatorV3)
          .map { complexities =>
            val verifierComplexity =
              dApp.internal.verifierFuncOpt
                .flatMap(verifier => complexities._2.get(verifier.u.name))
                .map(_.toInt)
                .getOrElse(0)
            new EstimateResult(
              verifierComplexity,
              complexities._1.toInt,
              complexities._2.view.mapValues(cost => new Integer(cost.toInt)).toMap.asJava
            )
          }
      case expression: Expression =>
        ExprScript
          .estimate(expression.internal, version, ScriptEstimatorV3, useContractVerifierLimit = !script.isAsset)
          .map(cost => new EstimateResult(cost.toInt, cost.toInt, new java.util.HashMap()))
      case _ => ???
    }
    cost.fold(error => throw new RideException(error), identity)
  }

  def extractMeta(script: Script): ScriptMeta =
    Global
      .dAppFuncTypes(script.base64String())
      .map { signatures =>
        val mappedSignatures =
          signatures.argsWithFuncName
            .map {
              case (functionName, arguments) =>
                val mappedArguments =
                  arguments.map {
                    case (name, argType) =>
                      new FunctionArgument(name, argType.toString)
                  }
                new FunctionSignature(functionName, mappedArguments.asJava)
            }
        new ScriptMeta(signatures.version, mappedSignatures.asJava)
      }
      .fold(error => throw new RideException(error.m), identity)
}

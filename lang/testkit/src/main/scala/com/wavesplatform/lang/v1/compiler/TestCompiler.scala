package com.wavesplatform.lang.v1.compiler

import cats.syntax.semigroup.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Asset, Expression, StdLibVersion, DApp as DAppType}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment

import scala.collection.mutable

class TestCompiler(version: StdLibVersion) {
  private lazy val baseCompilerContext =
    PureContext.build(version, useNewPowPrecision = true).withEnvironment[Environment] |+|
      CryptoContext.build(Global, version).withEnvironment[Environment]

  private lazy val compilerContext =
    (baseCompilerContext |+|
      WavesContext.build(Global, DirectiveSet(version, Account, DAppType).explicitGet())).compilerContext

  lazy val expressionContext: CTX[Environment] = WavesContext.build(Global, DirectiveSet(version, Account, Expression).explicitGet())
  private lazy val expressionCompilerContext =
    (baseCompilerContext |+|
      expressionContext).compilerContext

  private lazy val assetCompilerContext =
    (baseCompilerContext |+|
      WavesContext.build(Global, DirectiveSet(version, Asset, Expression).explicitGet())).compilerContext

  def compile(script: String, allowIllFormedStrings: Boolean = false): Either[String, DApp] =
    ContractCompiler.compile(script, compilerContext, version, allowIllFormedStrings = allowIllFormedStrings)

  def compileContract(script: String, allowIllFormedStrings: Boolean = false): ContractScriptImpl =
    ContractScript(version, compile(script, allowIllFormedStrings).explicitGet()).explicitGet()

  def compileExpression(script: String, allowIllFormedStrings: Boolean = false, checkSize: Boolean = true): ExprScript =
    ExprScript(
      version,
      ExpressionCompiler.compile(script, expressionCompilerContext, allowIllFormedStrings).explicitGet()._1,
      checkSize = checkSize
    ).explicitGet()

  def compileAsset(script: String): Script =
    ExprScript(version, ExpressionCompiler.compile(script, assetCompilerContext).explicitGet()._1).explicitGet()

  def compileFreeCall(script: String): ExprScript = {
    val expr = ContractCompiler.compileFreeCall(script, compilerContext, version).explicitGet()
    ExprScript(version, expr, isFreeCall = true).explicitGet()
  }
}

object TestCompiler {
  private val compilerByVersion = mutable.HashMap.empty[StdLibVersion, TestCompiler]
  def apply(version: StdLibVersion): TestCompiler =
    compilerByVersion.getOrElse(version, compilerByVersion.synchronized {
      compilerByVersion.getOrElseUpdate(version, new TestCompiler(version))
    })
}

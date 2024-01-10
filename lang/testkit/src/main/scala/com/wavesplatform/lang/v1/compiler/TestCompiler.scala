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
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries

import scala.collection.mutable

class TestCompiler(version: StdLibVersion) {
  private lazy val baseCompilerContext =
    PureContext.build(version, useNewPowPrecision = true) |+|
      CryptoContext.build(Global, version)

  lazy val dappContext: CTX =
    baseCompilerContext |+|
      WavesContext.build(Global, DirectiveSet(version, Account, DAppType).explicitGet(), fixBigScriptField = true)

  private lazy val expressionContext: CTX =
    WavesContext.build(Global, DirectiveSet(version, Account, Expression).explicitGet(), fixBigScriptField = true)
  private lazy val expressionCompilerContext =
    (baseCompilerContext |+|
      expressionContext).compilerContext

  private lazy val assetCompilerContext =
    (baseCompilerContext |+|
      WavesContext.build(Global, DirectiveSet(version, Asset, Expression).explicitGet(), fixBigScriptField = true)).compilerContext

  def compile(
      script: String,
      offset: LibrariesOffset = NoLibraries,
      allowIllFormedStrings: Boolean = false,
      compact: Boolean = false,
      removeUnused: Boolean = false
  ): Either[String, DApp] =
    ContractCompiler.compile(
      script,
      offset,
      dappContext.compilerContext,
      version,
      allowIllFormedStrings = allowIllFormedStrings,
      needCompaction = compact,
      removeUnusedCode = removeUnused
    )

  def compileContract(
      script: String,
      offset: LibrariesOffset = NoLibraries,
      allowIllFormedStrings: Boolean = false,
      compact: Boolean = false
  ): ContractScriptImpl =
    ContractScript(version, compile(script, offset, allowIllFormedStrings, compact).explicitGet()).explicitGet()

  def compileExpression(
      script: String,
      offset: LibrariesOffset = NoLibraries,
      allowIllFormedStrings: Boolean = false,
      checkSize: Boolean = true
  ): ExprScript =
    ExprScript(
      version,
      ExpressionCompiler.compile(script, offset, expressionCompilerContext, version, allowIllFormedStrings).explicitGet()._1,
      checkSize = checkSize
    ).explicitGet()

  def compileExpressionE(
      script: String,
      offset: LibrariesOffset = NoLibraries,
      allowIllFormedStrings: Boolean = false,
      checkSize: Boolean = true
  ): Either[String, ExprScript] =
    ExpressionCompiler
      .compile(script, offset, expressionCompilerContext, version, allowIllFormedStrings)
      .map(s =>
        ExprScript(
          version,
          s._1,
          checkSize = checkSize
        ).explicitGet()
      )

  def compileAsset(script: String, offset: LibrariesOffset = NoLibraries): Script =
    ExprScript(version, ExpressionCompiler.compile(script, offset, assetCompilerContext, version).explicitGet()._1).explicitGet()

  def compileFreeCall(script: String, offset: LibrariesOffset = NoLibraries): ExprScript = {
    val expr = ContractCompiler.compileFreeCall(script, offset, dappContext.compilerContext, version).explicitGet()
    ExprScript(version, expr, isFreeCall = true).explicitGet()
  }
}

object TestCompiler {
  private val compilerByVersion = mutable.HashMap.empty[StdLibVersion, TestCompiler]
  def apply(version: StdLibVersion): TestCompiler =
    compilerByVersion.getOrElse(
      version,
      compilerByVersion.synchronized {
        compilerByVersion.getOrElseUpdate(version, new TestCompiler(version))
      }
    )
}

package com.wavesplatform.lang.v1.compiler

import cats.syntax.semigroup._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, StdLibVersion, DApp => DAppType}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment

import scala.collection.mutable

class TestCompiler(version: StdLibVersion) {
  private lazy val compilerContext =
    (PureContext.build(version).withEnvironment[Environment] |+|
      CryptoContext.build(Global, version).withEnvironment[Environment] |+|
      WavesContext.build(Global, DirectiveSet(version, Account, DAppType).explicitGet())).compilerContext

  def compile(script: String): Either[String, DApp] =
    ContractCompiler.compile(script, compilerContext, version)

  def compileContract(script: String): Script =
    ContractScript(version, compile(script).explicitGet()).explicitGet()
}

object TestCompiler {
  private val compilerByVersion = mutable.HashMap.empty[StdLibVersion, TestCompiler]
  def apply(version: StdLibVersion): TestCompiler =
    compilerByVersion.getOrElse(version, compilerByVersion.synchronized {
      compilerByVersion.getOrElseUpdate(version, new TestCompiler(version))
    })
}
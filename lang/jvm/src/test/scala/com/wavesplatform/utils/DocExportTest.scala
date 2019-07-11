package com.wavesplatform.utils

import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import cats.implicits._
import com.wavesplatform.DocSource

class DocExportTest extends PropSpec with PropertyChecks with Matchers {
  val version = V3
  val cryptoCtx = CryptoContext.build(Global, version)
  val wavesCtx = WavesContext.build(DirectiveSet.contractDirectiveSet, Common.emptyBlockchainEnvironment())
  val pureCtx = PureContext.build(Global, V3)
  val fullCtx = cryptoCtx |+| wavesCtx |+| pureCtx

  property("all declared funcs and vars have doc") {
    fullCtx.vars.keys
      .map(DocSource.varData.get)
      .toList
      .sequence shouldBe defined

    fullCtx.functions
      .map(f => (f.name, f.signature.args.map(_._2.toString).toList))
      .map(DocSource.funcData.get)
      .toList
      .sequence shouldBe defined
  }
}

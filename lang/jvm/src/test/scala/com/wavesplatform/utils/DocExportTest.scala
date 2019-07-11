package com.wavesplatform.utils

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.{Common, ExecutionError, Global}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.directives.values.{Account, DApp, Expression, StdLibVersion, V1, V2, V3}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import cats.implicits._
import com.wavesplatform.DocSource

class DocExportTest extends PropSpec with PropertyChecks with Matchers {
  property("declared ride funcs and vars have doc for all versions") {
    DirectiveDictionary[StdLibVersion]
      .all
      .map(v => {
        val cryptoCtx = CryptoContext.build(Global, v)
        val wavesCtx = WavesContext.build(directives(v).explicitGet(), Common.emptyBlockchainEnvironment())
        val pureCtx = PureContext.build(Global, v)
        (cryptoCtx |+| wavesCtx |+| pureCtx, v)
      })
      .flatMap { case (ctx, ver) =>
        val vars = ctx.vars.keys
          .map(k => DocSource.varData.get((k, ver.value.asInstanceOf[Int])))

        val funcs = ctx.functions
          .map(f => (f.name, f.signature.args.map(_._2.toString).toList))
          .map(k => DocSource.funcData.get((k._1, k._2, ver.value.asInstanceOf[Int])))

        vars ++ funcs
      }
      .toList
      .sequence shouldBe defined
  }

  private def directives(v: StdLibVersion): Either[ExecutionError, DirectiveSet] = {
    val contentType = v match {
      case V1 | V2 => Expression
      case V3      => DApp
    }
    DirectiveSet(v, Account, contentType)
  }
}

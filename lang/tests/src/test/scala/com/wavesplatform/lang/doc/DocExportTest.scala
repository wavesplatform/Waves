package com.wavesplatform.lang.doc

import cats.implicits._
import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, StdLibVersion, _}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DocExportTest extends PropSpec with PropertyChecks with Matchers {

  def buildFullContext(ds: DirectiveSet): CTX[Environment] = {
    val wavesCtx  = WavesContext.build(Global, ds)
    val cryptoCtx = CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment]
    val pureCtx = PureContext.build(ds.stdLibVersion, fixUnicodeFunctions = true).withEnvironment[Environment]
    pureCtx |+| cryptoCtx |+| wavesCtx
  }

  for( ds <- directives ) {
    property(s"declared ride FUNCs have doc for $ds contexts") {

      val ctx = buildFullContext(ds)
      val totalFuncDocs = funcDoc(ctx, ds.stdLibVersion)

      totalFuncDocs.filter(_._1.isEmpty).foreach { v =>
        ctx.functions
        .filter(_.name == v._2)
        .foreach { f =>
          println((f.name, f.signature.args.map(_._2.toString).toList))
        }
      }
      val funcsWithoutDocInfo = totalFuncDocs.filter(_._1.isEmpty).map(_._2).mkString(", ")
      funcsWithoutDocInfo shouldBe ""
    }
  }

  property("declared ride VARs have doc for all contexts") {

    val totalVarDocs = for {
      ds <- directives
      ctx = buildFullContext(ds)
      doc <- varsDoc(ctx, ds.stdLibVersion)
    } yield doc

    val varsWithoutDocInfo = totalVarDocs.filter(_._1.isEmpty).map(_._2).mkString(", ")
    varsWithoutDocInfo shouldBe ""
  }

  lazy val directives: Seq[DirectiveSet] =
    DirectiveSet.contractDirectiveSet +:
      Set(V1, V2, V3, V4)
        .map(DirectiveSet(_, Account, Expression).explicitGet())
        .toSeq


  def varsDoc(ctx: CTX[Environment], ver: StdLibVersion): Iterable[(Option[String], String)] =
    ctx.vars.keys
      .map(k => (DocSource.varData.get((k, ver.value.asInstanceOf[Int])), k))

  def funcDoc(ctx: CTX[Environment], ver: StdLibVersion): Array[(Option[(String, List[String], Int)], String)] =
    ctx.functions
      .filterNot(_.name.startsWith("_"))
      .map(f => (f.name, f.signature.args.map(_._2.toString).toList))
      .map(k => (DocSource.funcData.get((k._1, k._2, ver.value.asInstanceOf[Int])), k._1))
}

package com.wavesplatform.utils

import cats.Id
import cats.implicits._
import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, StdLibVersion, _}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.utils.doc.RideFullContext
import org.scalatest.{FreeSpec, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DocExportTest extends PropSpec with PropertyChecks with Matchers {

  property("declared ride FUNCs have doc for all contexts") {

    val totalFuncDocs = for {
      ds <- directives
      ctx = RideFullContext.build(ds)
      doc <- funcDoc(ctx, ds.stdLibVersion)
    } yield doc

    val funcsWithoutDocInfo = totalFuncDocs.filter(_._1.isEmpty).map(_._2).mkString(", ")
    funcsWithoutDocInfo shouldBe ""
  }

  property("declared ride VARs have doc for all contexts") {

    val totalVarDocs = for {
      ds <- directives
      ctx = RideFullContext.build(ds)
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

  def funcDoc(ctx: CTX[Environment], ver: StdLibVersion): Array[(Option[(String, List[String])], String)] =
    ctx.functions
      .map(f => (f.name, f.signature.args.map(_._2.toString).toList))
      .map(k => (DocSource.funcData.get((k._1, k._2, ver.value.asInstanceOf[Int])), k._1))
}

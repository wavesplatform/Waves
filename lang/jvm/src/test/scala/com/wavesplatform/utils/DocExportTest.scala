package com.wavesplatform.utils

import cats.implicits._
import com.wavesplatform.DocSource
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, StdLibVersion, _}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.utils.doc.RideFullContext
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DocExportTest extends PropSpec with PropertyChecks with Matchers {
  property("declared ride funcs and vars have doc for all contexts") {
    val totalDocs = for {
      ds <- directives
      ctx = RideFullContext.build(ds)
      doc <- varsDoc(ctx, ds.stdLibVersion) ++ funcDoc(ctx, ds.stdLibVersion)
    } yield doc

    totalDocs.toList.sequence shouldBe defined
  }

  lazy val directives: Seq[DirectiveSet] =
    DirectiveSet.contractDirectiveSet +:
      Set(V1, V2, V3)
        .map(DirectiveSet(_, Account, Expression).explicitGet())
        .toSeq


  def varsDoc(ctx: CTX, ver: StdLibVersion): Iterable[Option[String]] =
    ctx.vars.keys
      .map(k => DocSource.varData.get((k, ver.value.asInstanceOf[Int])))

  def funcDoc(ctx: CTX, ver: StdLibVersion): Array[Option[(String, List[String])]] =
    ctx.functions
      .map(f => (f.name, f.signature.args.map(_._2.toString).toList))
      .map(k => DocSource.funcData.get((k._1, k._2, ver.value.asInstanceOf[Int])))
}

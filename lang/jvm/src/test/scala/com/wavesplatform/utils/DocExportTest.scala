package com.wavesplatform.utils

import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import cats.implicits._
import com.wavesplatform.DocSource
import com.wavesplatform.utils.doc.RideFullContext

class DocExportTest extends PropSpec with PropertyChecks with Matchers {
  property("declared ride funcs and vars have doc for all versions") {
    DirectiveDictionary[StdLibVersion]
      .all
      .map(ver => (RideFullContext.build(ver), ver))
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
}

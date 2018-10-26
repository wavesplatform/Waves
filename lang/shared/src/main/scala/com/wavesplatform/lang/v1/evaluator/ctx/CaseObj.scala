package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF

case class CaseObj(caseType: CASETYPEREF, fields: Map[String, EVALUATED]) extends EVALUATED{
  override def toString: String = {
    s"""
       |${caseType.name} {
       |  ${fields.map({ case (k, v) => s"$k -> $v" }).mkString(", ")}
       |}
     """.stripMargin
  }
}

case class ARR(xs: IndexedSeq[EVALUATED]) extends EVALUATED

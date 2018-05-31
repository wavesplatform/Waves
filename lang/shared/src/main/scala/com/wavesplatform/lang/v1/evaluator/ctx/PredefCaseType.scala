package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.{CASETYPEREF, TYPE, UNION}
import com.wavesplatform.lang.v1.compiler.CompilerContext

trait PredefBase {
  def name: String
  def fields: List[(String, TYPE)]
  def typeRef: TYPE
}

case class PredefCaseType(name: String, fields: List[(String, TYPE)]) extends PredefBase {
  lazy val typeRef = CASETYPEREF(name)
}

case class UnionType(name: String, types: List[CASETYPEREF], ctx: CompilerContext) extends PredefBase {
  lazy val typeRef = UNION(types)
  lazy val fields = types.map(n => ctx.predefTypes(n.name).fields.toSet).reduce(_ intersect _).toList
}

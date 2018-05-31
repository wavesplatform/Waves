package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.{CASETYPEREF, TYPE, UNION}

trait PredefBase {
  def name: String
  def typeRef: TYPE
}

case class PredefCaseType(name: String, fields: List[(String, TYPE)]) extends PredefBase {
  lazy val typeRef = CASETYPEREF(name)
}

case class UnionType(name: String, types: List[CASETYPEREF]) extends PredefBase {
  lazy val typeRef = UNION(types)
}

package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, TYPE, UNION}

trait DefinedType {
  def name: String
  def typeRef: TYPE
}

case class CaseType(name: String, fields: List[(String, TYPE)]) extends DefinedType {
  lazy val typeRef = CASETYPEREF(name)
}

case class UnionType(name: String, types: List[CASETYPEREF]) extends DefinedType {
  lazy val typeRef = UNION(types)
}

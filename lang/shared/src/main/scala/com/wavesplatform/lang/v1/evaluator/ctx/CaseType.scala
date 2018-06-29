package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL, REAL, UNION}

trait DefinedType {
  def name: String
  def typeRef: FINAL
}

case class CaseType(name: String, fields: List[(String, FINAL)]) extends DefinedType {
  lazy val typeRef = CASETYPEREF(name, fields)
}

case class UnionType(name: String, types: List[REAL]) extends DefinedType {
  lazy val typeRef = UNION.create(types)
}

package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL}

trait DefinedType {
  def name: String
  def typeRef: FINAL
  def fields: List[(String, FINAL)]
}

case class CaseType(name: String, fields: List[(String, FINAL)]) extends DefinedType {
  lazy val typeRef = CASETYPEREF(name, fields)
}

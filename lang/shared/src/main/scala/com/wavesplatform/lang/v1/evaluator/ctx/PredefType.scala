package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.{CASETYPEREF, TYPE, TYPEREF}

trait PredefBase {
  def name: String
  def fields: List[(String, TYPE)]
  def typeRef: TYPE
}

case class PredefType(name: String, fields: List[(String, TYPE)]) extends PredefBase {
  lazy val typeRef = TYPEREF(name)
}
case class PredefCaseType(name: String, fields: List[(String, TYPE)]) extends PredefBase {
  lazy val typeRef = CASETYPEREF(name)
}

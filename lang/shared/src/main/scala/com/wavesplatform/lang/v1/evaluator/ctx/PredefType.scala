package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.{CASETYPEREF, TYPE, TYPEREF}

case class PredefType(name: String, fields: List[(String, TYPE)]) {
  lazy val typeRef = TYPEREF(name)
}
case class PredefCaseType(name: String, fields: List[(String, TYPE)]) {
  lazy val typeRef = CASETYPEREF(name)
}

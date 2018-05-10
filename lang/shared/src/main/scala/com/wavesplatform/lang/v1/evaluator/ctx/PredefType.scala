package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.compiler.Terms.{TYPE, TYPEREF}

case class PredefType(name: String, fields: List[(String, TYPE)]) {
  lazy val typeRef = TYPEREF(name)
}

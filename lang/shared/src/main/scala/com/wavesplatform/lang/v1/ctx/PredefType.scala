package com.wavesplatform.lang.v1.ctx

import com.wavesplatform.lang.v1.parser.Terms.{TYPE, TYPEREF}

case class PredefType(name: String, fields: List[(String, TYPE)]) {
  lazy val typeRef = TYPEREF(name)
}

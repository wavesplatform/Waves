package com.wavesplatform.lang.ctx

import com.wavesplatform.lang.Terms.{TYPE, TYPEREF}

case class PredefType(name: String, fields: List[(String, TYPE)]) {
  lazy val typeRef = TYPEREF(name)
}


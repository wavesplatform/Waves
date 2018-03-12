package com.wavesplatform.lang.ctx

import com.wavesplatform.lang.Terms.TYPE

case class PredefType(name: String, fields: List[(String, TYPE)])


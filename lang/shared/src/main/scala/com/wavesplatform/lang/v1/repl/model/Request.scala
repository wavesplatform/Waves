package com.wavesplatform.lang.v1.repl.model

trait Request {
  type Domain
  def domain: Domain
}

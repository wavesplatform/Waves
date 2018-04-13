package com.wavesplatform.lang

trait Versioned {
  type V <: ScriptVersion
  val version: V
}

package com.wavesplatform.lang

trait Versioned {
  type Ver <: ScriptVersion
  val version: Ver
}

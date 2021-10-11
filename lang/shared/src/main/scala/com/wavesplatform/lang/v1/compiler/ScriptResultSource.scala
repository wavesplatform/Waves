package com.wavesplatform.lang.v1.compiler

sealed abstract class ScriptResultSource(val name: String)
object ScriptResultSource {
  object CallableFunction extends ScriptResultSource("CallableFunction")
  object FreeCall         extends ScriptResultSource("FreeCall")
}

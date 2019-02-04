package com.wavesplatform.lang.directives

sealed trait DirectiveKey
object DirectiveKey {
  final case object STDLIB_VERSION extends DirectiveKey
  final case object SCRIPT_TYPE extends DirectiveKey

  val dictionary =
    Map(
      "STDLIB_VERSION" -> DirectiveKey.STDLIB_VERSION,
      "SCRIPT_TYPE" -> DirectiveKey.SCRIPT_TYPE
    )
}

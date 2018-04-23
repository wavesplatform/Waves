package com.wavesplatform.lang.directives

sealed trait DirectiveKey
object DirectiveKey {
  final case object LANGUAGE_VERSION extends DirectiveKey

  val dictionary =
    Map(
      "LANGUAGE_VERSION" -> DirectiveKey.LANGUAGE_VERSION
    )
}

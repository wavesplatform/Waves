package com.wavesplatform.lang.directives.values

import com.wavesplatform.lang.directives._

abstract class DirectiveValue(
    val key:  DirectiveKey,
    val text: String,
    val id:   Int
) {
    lazy val unparsed: String = s"${DirectiveParser.start} ${key.text} $text ${DirectiveParser.end}"
    val value: Any
}

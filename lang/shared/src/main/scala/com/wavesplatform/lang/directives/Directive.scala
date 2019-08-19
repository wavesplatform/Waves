package com.wavesplatform.lang.directives

import com.wavesplatform.lang.directives.values.DirectiveValue
import com.wavesplatform.lang.directives.DirectiveKey._

case class Directive(key: DirectiveKey, value: DirectiveValue)

object Directive {
  def extractValue(directives: Iterable[Directive], key: DirectiveKey): key.Value =
    directives
      .find(_.key == key)
      .map(_.value)
      .getOrElse(key match {
        case k: PredefinedDirectiveKey => k.valueDic.default
        case k: ArbitraryDirectiveKey  => k.valueMapper("")
      })
      .asInstanceOf[key.Value]

  def extractDirectives(directives: Iterable[Directive]): Either[String, DirectiveSet] =
    DirectiveSet(
      extractValue(directives, STDLIB_VERSION),
      extractValue(directives, SCRIPT_TYPE),
      extractValue(directives, CONTENT_TYPE),
      extractValue(directives, IMPORT)
    )
}

package com.wavesplatform.lang.directives

import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.DirectiveKey.*

case class Directive(key: DirectiveKey, value: DirectiveValue)

object Directive {
  def extractValue(directives: Iterable[Directive], key: DirectiveKey)(implicit default: => Option[key.Value] = None): key.Value =
    directives
      .find(_.key == key)
      .map(_.value)
      .orElse(default)
      .getOrElse(key match {
        case k: PredefinedDirectiveKey => k.valueDic.default
        case k: ArbitraryDirectiveKey  => k.valueMapper("")
      })
      .asInstanceOf[key.Value]

  def extractDirectives(
      directives: Iterable[Directive],
      defaultStdLib: => STDLIB_VERSION.Value = StdLibVersion.VersionDic.default
  ): Either[String, DirectiveSet] =
    DirectiveSet(
      directives.find(_.key == STDLIB_VERSION).fold(defaultStdLib)(_.value.asInstanceOf[STDLIB_VERSION.Value]),
      extractValue(directives, SCRIPT_TYPE)(None),
      extractValue(directives, CONTENT_TYPE)(None),
      extractValue(directives, IMPORT)(None)
    )
}

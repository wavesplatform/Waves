package com.wavesplatform.lang.directives

import com.wavesplatform.lang.directives.values._
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

      def extractDirectives(directives: Iterable[Directive], defaultStdLib: => STDLIB_VERSION.Value = StdLibVersion.VersionDic.default): Either[String, DirectiveSet] =
    DirectiveSet(
      directives.find(_.key == STDLIB_VERSION).fold(defaultStdLib)(_.value.asInstanceOf[STDLIB_VERSION.Value]),
      extractValue(directives, SCRIPT_TYPE),
      extractValue(directives, CONTENT_TYPE),
      extractValue(directives, IMPORT)
    )
}

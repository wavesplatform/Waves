package com.wavesplatform.lang.directives

import com.wavesplatform.lang.directives.values.DirectiveValue
import com.wavesplatform.lang.directives.DirectiveKey._

case class Directive(key: DirectiveKey, value: DirectiveValue)

object Directive {
  def extractValue(
      directives: Iterable[Directive],
      key:        DirectiveKey
  )(implicit dic: DirectiveDictionary[key.Value]): key.Value =
    directives
      .find(_.key == key)
      .map(_.value.asInstanceOf[key.Value])
      .getOrElse(dic.default)

  def extractDirectives(directives: Iterable[Directive]): Either[String, DirectiveSet] =
    DirectiveSet(
      extractValue(directives, STDLIB_VERSION),
      extractValue(directives, SCRIPT_TYPE),
      extractValue(directives, CONTENT_TYPE)
    )
}

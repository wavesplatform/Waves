package com.wavesplatform.lang.directives.values

import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.DirectiveKey.CONTENT_TYPE

sealed abstract class ContentType(text: String, id: Int) extends DirectiveValue(CONTENT_TYPE, text, id) {
  override val value: Any = text
}
case object Expression extends ContentType("EXPRESSION", 1)
case object DApp       extends ContentType("DAPP", 2)

object ContentType {
  implicit val dictionary: DirectiveDictionary[ContentType] = new {
    val default: ContentType      = Expression
    val all:     Set[ContentType] = Set(Expression, DApp)
  } with DirectiveDictionary[ContentType]

  def isDApp(isDApp: Boolean): ContentType = if (isDApp) DApp else Expression
}

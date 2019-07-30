package com.wavesplatform.lang.directives.values

import com.wavesplatform.lang.directives.DirectiveDictionary

sealed abstract class ContentType(text: String, id: Int) extends DirectiveValue(text, id) {
  override val value: Any = text
  override def key        = resolveKey[ContentType]
}
case object Expression extends ContentType("EXPRESSION", 1)
case object DApp       extends ContentType("DAPP", 2)
case object Library    extends ContentType("LIBRARY", 3)

object ContentType {
  implicit object ContentDic extends DirectiveDictionary[ContentType] {
    override val default: ContentType           = Expression
    override val all:     Iterable[ContentType] = Seq(Expression, DApp, Library)
  }

  def isDApp(isDApp: Boolean): ContentType = if (isDApp) DApp else Expression
}

package com.wavesplatform.lang.directives.values

import com.wavesplatform.lang.directives.*

case class Imports(fileNames: List[String] = Nil) extends DirectiveValue(fileNames.mkString(", "), 0) {
  override def key: DirectiveKey = DirectiveKey.IMPORT
  override val value: Any        = text
}

object Imports {
  implicit val mapper: String => Imports = s =>
    if (s == "") Imports()
    else Imports(s.split(",").map(_.trim).toList)
}

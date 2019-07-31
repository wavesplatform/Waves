package com.wavesplatform.lang.directives.values

import com.wavesplatform.lang.directives._

case class Imports(fileNames: List[String] = Nil) extends DirectiveValue(fileNames.mkString(", "), 0) {
  override def key: DirectiveKey = resolveKey[Imports]
  override val value: Any        = text
}

object Imports {
  implicit val mapper: String => Imports = s =>
    if (s == "") Imports()
    else Imports(s.split(",").map(_.trim).toList)
}

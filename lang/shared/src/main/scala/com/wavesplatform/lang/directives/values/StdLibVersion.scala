package com.wavesplatform.lang.directives.values
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.DirectiveKey.STDLIB_VERSION

sealed abstract class StdLibVersion(id: Int) extends DirectiveValue(STDLIB_VERSION, id.toString, id)  {
  override val value: Any = id
}
case object V1 extends StdLibVersion(1)
case object V2 extends StdLibVersion(2)
case object V3 extends StdLibVersion(3)

object StdLibVersion {
    implicit val dictionary: DirectiveDictionary[StdLibVersion] = new {
      override val default: StdLibVersion      = V2
      override val all:     Iterable[StdLibVersion] = Seq(V1, V2, V3)
    } with DirectiveDictionary[StdLibVersion]
}

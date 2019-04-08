package com.wavesplatform.lang.directives.values
import cats.implicits._
import com.wavesplatform.lang.directives.DirectiveDictionary

sealed abstract class StdLibVersion(id: Int) extends DirectiveValue(id.toString, id) {
  override val value: Any = id
  override def key        = resolveKey[StdLibVersion]
}
case object V1 extends StdLibVersion(1)
case object V2 extends StdLibVersion(2)
case object V3 extends StdLibVersion(3)
case object V4 extends StdLibVersion(4)

object StdLibVersion {
  implicit object VersionDic extends DirectiveDictionary[StdLibVersion] {
    override val default: StdLibVersion       = V2
    override val all: Iterable[StdLibVersion] = Seq(V1, V2, V3, V4)
  }

  implicit val stdLibVersionOrdering: Ordering[StdLibVersion] = Ordering.by(_.id)
}

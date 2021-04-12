package com.wavesplatform.lang.directives.values
import com.wavesplatform.lang.directives.DirectiveDictionary

sealed abstract class StdLibVersion(id: Int) extends DirectiveValue(id.toString, id) with Product with Serializable with Ordered[StdLibVersion] {
  override val value: Any = id
  override def key        = resolveKey[StdLibVersion]

  override def compare(that: StdLibVersion): Int = id compare that.id
}
case object V1 extends StdLibVersion(1)
case object V2 extends StdLibVersion(2)
case object V3 extends StdLibVersion(3)
case object V4 extends StdLibVersion(4)
case object V5 extends StdLibVersion(5)

object StdLibVersion {
  implicit object VersionDic extends DirectiveDictionary[StdLibVersion] {
    override val default: StdLibVersion       = V3
    override val all: Iterable[StdLibVersion] = Seq(V1, V2, V3, V4, V5)
  }

  val V1: StdLibVersion = com.wavesplatform.lang.directives.values.V1
  val V2: StdLibVersion = com.wavesplatform.lang.directives.values.V2
  val V3: StdLibVersion = com.wavesplatform.lang.directives.values.V3
  val V4: StdLibVersion = com.wavesplatform.lang.directives.values.V4
  val V5: StdLibVersion = com.wavesplatform.lang.directives.values.V5
}

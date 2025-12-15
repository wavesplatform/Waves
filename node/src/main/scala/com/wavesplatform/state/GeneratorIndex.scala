package com.wavesplatform.state

opaque type GeneratorIndex = Int
object GeneratorIndex {
  def apply(x: Int): GeneratorIndex = {
    require(x >= 0, s"Wrong generator index: $x. Expected >= 0")
    x
  }

  def checked(x: Int): Option[GeneratorIndex] = {
    require(x >= -1, s"Wrong generator index: $x. Expected -1 or >= 0")
    Option.when(x >= 0)(x)
  }

  def seq(xs: Seq[Int]): Seq[GeneratorIndex]       = xs.map(apply)
  def unsafeSeq(xs: Seq[Int]): Seq[GeneratorIndex] = xs

  extension (self: GeneratorIndex) {
    def toInt: Int = self
  }

  def toInts(xs: Seq[GeneratorIndex]): Seq[Int] = xs

  given Ordering[GeneratorIndex]                            = Ordering[Int]
  given Conversion[GeneratorIndex, Ordered[GeneratorIndex]] = scala.math.Ordered.orderingToOrdered(_)
}

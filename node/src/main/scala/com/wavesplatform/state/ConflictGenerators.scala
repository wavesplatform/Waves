package com.wavesplatform.state

import com.wavesplatform.state

import scala.collection.Searching.*

case class ConflictGenerators private (private val heights: Vector[Height], private val generators: Vector[Seq[GeneratorIndex]]) {
  def append(h: Height, idx: GeneratorIndex): ConflictGenerators = {
    require(heights.isEmpty || implicitly[Ordering[Height]].lt(heights.last, h), s"height $h must increase, last height: ${heights.last}")
    appendAllUnsafe(h, Seq(idx))
  }

  def appendAll(h: Height, idxs: Seq[GeneratorIndex]): ConflictGenerators = {
    require(heights.isEmpty || implicitly[Ordering[Height]].lt(heights.last, h), s"height $h must increase, last height: ${heights.last}")
    appendAllUnsafe(h, idxs)
  }

  private def appendAllUnsafe(h: Height, idxs: Seq[GeneratorIndex]): ConflictGenerators = copy(
    heights = heights :+ h,
    generators =
      if (generators.isEmpty) Vector(idxs)
      else generators.init :+ (generators.last ++ idxs)
  )

  def upTo(h: Height): Set[GeneratorIndex] = {
    val idx = heights.search(h) match {
      case Found(i)          => i
      case InsertionPoint(i) => i - 1
    }
    if (idx < 0) Set.empty
    else generators.view.take(idx + 1).flatten.toSet
  }

  def deleteLastIf(expected: Height): ConflictGenerators =
    if (heights.nonEmpty && heights.last == expected) copy(heights = heights.init, generators = generators.init)
    else this
}

object ConflictGenerators {
  val empty = ConflictGenerators(Vector.empty, Vector.empty)
}

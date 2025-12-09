package com.wavesplatform.state

import com.wavesplatform.state

import scala.collection.Searching.*
import scala.collection.View

/** Stores indexes of conflict generators by height in one generation period */
case class ConflictGenerators private (private val heights: Vector[Height], private val generators: Vector[Seq[GeneratorIndex]]) {
  def appendAll(h: Height, idxs: GeneratorIndex*): ConflictGenerators = {
    require(
      idxs.isEmpty || heights.isEmpty || implicitly[Ordering[Height]].lt(heights.last, h),
      s"height $h must increase, last height: ${heights.last}"
    )
    if (idxs.isEmpty) this
    else
      copy(
        heights = heights :+ h,
        generators = generators :+ idxs
      )
  }

  def size: Int        = generators.foldLeft(0)(_ + _.size)
  def isEmpty: Boolean = generators.isEmpty

  def heightOf(idx: GeneratorIndex): Option[Height] = heights.view.zip(generators).collectFirst { case (h, idxs) if idxs.contains(idx) => h }

  def all: Set[GeneratorIndex]                           = generators.view.flatten.toSet
  def upTo(h: Height): Set[GeneratorIndex]               = upToView(h).toSet
  def hasInUpTo(h: Height, idx: GeneratorIndex): Boolean = upToView(h).exists(_ == idx)

  private def upToView(h: Height): View[GeneratorIndex] = {
    val idx = heights.search(h) match {
      case Found(i)          => i
      case InsertionPoint(i) => i - 1
    }
    if (idx < 0) View.empty
    else generators.view.take(idx + 1).flatten
  }

  def deleteLastIf(expected: Height): ConflictGenerators =
    if (heights.nonEmpty && heights.last == expected) copy(heights = heights.init, generators = generators.init)
    else this
}

object ConflictGenerators {
  val empty = ConflictGenerators(Vector.empty, Vector.empty)
}

package com.wavesplatform.state2

trait Versioned[T] {
  val codeVersion: Int
  def readVersion(t: T): Option[Int]
  def persistVersion(t: T, vesrion: Int)
}

package com.wavesplatform.ride.blockchain

case class WithHeight[A](height: Int, value: A) {
  def map[B](f: A => B): WithHeight[B] = WithHeight(height, f(value))
}

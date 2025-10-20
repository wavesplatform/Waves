package com.wavesplatform.test

trait TestUtils {

  /** Creates an inner scope and prevents name clashing */
  def isolated[A](f: => A): A = f
}

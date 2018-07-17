package com.wavesplatform.generator.utils

import java.util.concurrent.ThreadLocalRandom

object Implicits {
  final implicit class IteratorUtilsOps(val self: Iterator.type) extends AnyVal {
    private def random = ThreadLocalRandom.current

    def randomContinually[A](orig: Seq[A]): Iterator[A] = new Iterator[A] {
      private val origSize = orig.size

      override val hasNext: Boolean = true
      override def next(): A        = orig(random.nextInt(origSize))
    }
  }
}

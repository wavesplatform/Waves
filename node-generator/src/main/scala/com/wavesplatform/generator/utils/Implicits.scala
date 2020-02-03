package com.wavesplatform.generator.utils

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.settings.Constants

object Implicits {
  final implicit class IteratorUtilsOps(val self: Iterator.type) extends AnyVal {
    private def random = ThreadLocalRandom.current

    def randomContinually[A](orig: Seq[A]): Iterator[A] = new Iterator[A] {
      private val origSize = orig.size

      override val hasNext: Boolean = true
      override def next(): A        = orig(random.nextInt(origSize))
    }
  }
  final implicit class DoubleExt(val d: Double) extends AnyVal {
    def waves: Long = (BigDecimal(d) * Constants.UnitsInWave).toLong
  }
}

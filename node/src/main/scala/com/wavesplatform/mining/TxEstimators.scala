package com.wavesplatform.mining

import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.Transaction

object TxEstimators {
  abstract class Fn extends ((Blockchain, Transaction, Diff) => Long) {
    val minEstimate: Long
  }

  object sizeInBytes extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction, diff: Diff): Long = x.bytes().length // + headers

    override def toString(): String = "sizeInBytes"

    override val minEstimate = 109L
  }

  object one extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction, diff: Diff): Long = 1

    override def toString(): String = "one"

    override val minEstimate = 1L
  }

  object scriptRunNumber extends Fn {
    override def apply(blockchain: Blockchain, x: Transaction, diff: Diff): Long = {
      diff.scriptsRun
    }

    override def toString(): String = "scriptRunNumber"

    override val minEstimate = 0L
  }
}

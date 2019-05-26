package com.wavesplatform.mining

import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging

//noinspection ScalaStyle
object TxEstimators extends ScorexLogging {
  trait Fn {
    def apply(blockchain: Blockchain, transaction: Transaction, diff: Diff): Long
    def minEstimate: Long
  }

  case object sizeInBytes extends Fn {
    override def apply(blockchain: Blockchain, tx: Transaction, diff: Diff): Long = tx.bytes().length // + headers
    override val minEstimate                                                      = 109L
  }

  case object one extends Fn {
    override def apply(blockchain: Blockchain, tx: Transaction, diff: Diff): Long = 1
    override val minEstimate                                                      = 1L
  }

  case object scriptRunNumber extends Fn {
    override def apply(blockchain: Blockchain, tx: Transaction, diff: Diff): Long = diff.scriptsRun
    override val minEstimate                                                      = 0L
  }

  case object scriptsComplexity extends Fn {
    override def apply(blockchain: Blockchain, tx: Transaction, diff: Diff): Long = diff.scriptsComplexity
    override val minEstimate                                                      = 0L
  }
}

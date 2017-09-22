package com.wavesplatform.generator

import java.util.concurrent.atomic.AtomicReference

import cats.Show
import com.wavesplatform.generator.DynamicWideTransactionGenerator.Settings
import com.wavesplatform.generator.utils.Gen
import scorex.account.PrivateKeyAccount
import scorex.transaction.Transaction

class DynamicWideTransactionGenerator(settings: Settings,
                                      accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  require(accounts.nonEmpty)

  private val nextTxsNumber = new AtomicReference[Double](settings.start)

  override def next(): Iterator[Transaction] = {
    val currTxsNumber = nextTxsNumber
      .getAndUpdate { x =>
        val newValue = x + settings.growAdder
        settings.maxTxsPerRequest.foldLeft(newValue)(Math.max(_, _))
      }
      .toInt
    Gen.txs(accounts).take(currTxsNumber)
  }

}

object DynamicWideTransactionGenerator {

  case class Settings(start: Int,
                      growAdder: Double,
                      maxTxsPerRequest: Option[Int],
                      limitDestAccounts: Option[Int]) {
    require(start >= 1)
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""transactions at start: $start
         |grow factor: $growAdder""".stripMargin
    }
  }

}

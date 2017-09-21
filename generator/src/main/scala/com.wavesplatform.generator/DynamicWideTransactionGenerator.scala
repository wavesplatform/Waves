package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicReference

import com.wavesplatform.generator.DynamicWideTransactionGenerator.Settings
import com.wavesplatform.generator.utils.Implicits._
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{Transaction, TransactionParser}

import scala.util.Random

class DynamicWideTransactionGenerator(settings: Settings,
                                      accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  require(accounts.nonEmpty)

  private def random = Random.javaRandomToRandom(ThreadLocalRandom.current)

  private val senderGen = Iterator.randomContinually(accounts)

  private val recipientGen = Iterator.continually {
    val pk = Array.fill[Byte](TransactionParser.KeyLength)(random.nextInt(Byte.MaxValue).toByte)
    Address.fromPublicKey(pk)
  }

  private val maxFee = 100000
  private val feeGen = Iterator.continually(random.nextInt(maxFee) + 1)

  private val txsGen = senderGen.zip(recipientGen).zip(feeGen)
    .map {
      case ((src, dst), fee) =>
        TransferTransaction.create(None, src, dst, fee, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
    }
    .collect { case Right(x) => x }

  private val nextTxsNumber = new AtomicReference[Double](settings.start)

  override val hasNext = true
  override def next(): Iterator[Transaction] = {
    val currTxsNumber = nextTxsNumber.getAndUpdate(_ * settings.growFactor).toInt
    txsGen.take(currTxsNumber)
  }

}

object DynamicWideTransactionGenerator {
  case class Settings(start: Int,
                      growFactor: Double,
                      limitDestAccounts: Option[Int]) {
    require(start >= 1)
    require(growFactor > 0)
  }
}

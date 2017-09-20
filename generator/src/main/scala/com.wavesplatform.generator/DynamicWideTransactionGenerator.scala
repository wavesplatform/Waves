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
                                      accounts: Seq[PrivateKeyAccount]) extends Iterator[Iterator[Transaction]] {
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

  private val nextTxsNumber = new AtomicReference[Float](settings.start)

  override val hasNext = true
  override def next(): Iterator[Transaction] = {
    val currTxsNumber = nextTxsNumber.getAndUpdate(_ * settings.grow).toInt
    txsGen.take(currTxsNumber)
  }

}

object DynamicWideTransactionGenerator {
  case class Settings(start: Int,
                      grow: Float,
                      limitDestAccounts: Option[Int]) {
    require(start >= 1)
    require(grow > 0)
  }
}

/*
class GrowingTransactionGenerator(numberPerIteration: Int,
                                  limitDestAccounts: Option[Int],
                                  accounts: Seq[PrivateKeyAccount]) extends Iterator[Seq[Transaction]] {
  require(accounts.nonEmpty)

  private def random = Random.javaRandomToRandom(ThreadLocalRandom.current)

  private val senders = randomContinually(accounts)

  private val recipients = Iterator.continually {
    val seedSize = 32
    val pk = Array.fill[Byte](seedSize)(random.nextInt(Byte.MaxValue).toByte)
    Address.fromPublicKey(pk)
  }

  private def randomContinually[A](orig: Seq[A]): Iterator[A] = new Iterator[A] {
    private def random = ThreadLocalRandom.current()
    private val origSize = orig.size

    override val hasNext: Boolean = true
    override def next(): A = orig(random.nextInt(origSize))
  }

  override val hasNext = true
  override def next(): Seq[Transaction] = {
    val fee = 100000
    val finalTxsNumber = Math.min(limitDestAccounts.getOrElse(numberPerIteration), numberPerIteration)

    val sourcesAndDestinations = senders.zip(recipients).take(finalTxsNumber)
    sourcesAndDestinations.foldLeft(List.empty[TransferTransaction]) {
      case (txs, (src, dst)) =>
        val amount = random.nextInt(fee) + 1
        val ts = System.currentTimeMillis()

        val maybeTransaction = TransferTransaction.create(None, src, dst, amount, ts, None, fee, Array.emptyByteArray)
        maybeTransaction.fold(_ => txs, _ :: txs)
    }
  }

}
 */
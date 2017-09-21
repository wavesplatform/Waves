package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.Transaction
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class WideTransactionGenerator(limitDstAccounts: Option[Int],
                               val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  require(accounts.nonEmpty)

  override def generate(count: Int): Seq[Transaction] = {
    val random = Random.javaRandomToRandom(ThreadLocalRandom.current)

    val fee = 100000
    val seedSize = 32

    val srcAccounts = randomContinually(accounts)
    val dstAccounts = Iterator.continually {
      val pk = Array.fill[Byte](seedSize)(random.nextInt(Byte.MaxValue).toByte)
      Address.fromPublicKey(pk)
    }

    val finalTxsNumber = Math.min(limitDstAccounts.getOrElse(count), count)
    val sourcesAndDestinations = srcAccounts.zip(dstAccounts).take(finalTxsNumber)
    sourcesAndDestinations.foldLeft(List.empty[TransferTransaction]) {
      case (txs, (src, dst)) =>
        val amount = random.nextInt(fee) + 1
        val ts = System.currentTimeMillis()
        val maybeTransaction = TransferTransaction.create(None, src, dst, amount, ts, None, fee, Array.emptyByteArray)
        if (maybeTransaction.isRight) txs :+ maybeTransaction.right.get else txs
    }
  }

  def randomContinually[A](orig: Seq[A]): Iterator[A] = new Iterator[A] {
    private def random = ThreadLocalRandom.current()
    private val origSize = orig.size

    override def hasNext: Boolean = true
    override def next(): A = orig(random.nextInt(origSize))
  }
}

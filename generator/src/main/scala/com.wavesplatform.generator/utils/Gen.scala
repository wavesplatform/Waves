package com.wavesplatform.generator.utils

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.generator.utils.Implicits._
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{Transaction, TransactionParser}

object Gen {
  private def random = ThreadLocalRandom.current

  def txs(minFee: Long,
          maxFee: Long,
          senderAccounts: Seq[PrivateKeyAccount],
          recipientGen: Iterator[Address]): Iterator[Transaction] = {
    val senderGen = Iterator.randomContinually(senderAccounts)
    val feeGen = Iterator.continually(minFee + random.nextLong(maxFee - minFee))

    senderGen.zip(recipientGen).zip(feeGen)
      .map {
        case ((src, dst), fee) =>
          TransferTransaction.create(None, src, dst, fee, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
      }
      .collect { case Right(x) => x }
  }

  val address: Iterator[Address] = Iterator.continually {
    val pk = Array.fill[Byte](TransactionParser.KeyLength)(random.nextInt(Byte.MaxValue).toByte)
    Address.fromPublicKey(pk)
  }

  def address(uniqNumber: Int): Iterator[Address] = Iterator.randomContinually(address.take(uniqNumber).toSeq)

  def address(limitUniqNumber: Option[Int]): Iterator[Address] = limitUniqNumber.map(address(_)).getOrElse(address)

}

package com.wavesplatform.generator.utils

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.generator.utils.Implicits._
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.assets.MassTransferTransaction.ParsedTransfer
import scorex.transaction.assets.{MassTransferTransaction, TransferTransaction}
import scorex.transaction.{Transaction, TransactionParser}

object Gen {
  private def random = ThreadLocalRandom.current

  def txs(minFee: Long,
          maxFee: Long,
          senderAccounts: Seq[PrivateKeyAccount],
          recipientGen: Iterator[Address]): Iterator[Transaction] = {
    val senderGen = Iterator.randomContinually(senderAccounts)
    val feeGen = Iterator.continually(minFee + random.nextLong(maxFee - minFee))
    transfers(senderGen, recipientGen, feeGen)
      .zip(massTransfers(senderGen, recipientGen, feeGen))
      .flatMap { case (tx1, tx2) => Iterator(tx1, tx2) }
  }

  def transfers(senderGen: Iterator[PrivateKeyAccount],
                recipientGen: Iterator[Address],
                feeGen: Iterator[Long]): Iterator[Transaction] = {
    senderGen.zip(recipientGen).zip(feeGen)
      .map {
        case ((src, dst), fee) =>
          TransferTransaction.create(None, src, dst, fee, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
      }
      .collect { case Right(x) => x }
  }

  def massTransfers(senderGen: Iterator[PrivateKeyAccount],
                    recipientGen: Iterator[Address],
                    amountGen: Iterator[Long]): Iterator[Transaction] = {
    val transferCountGen = Iterator.continually(random.nextInt(MassTransferTransaction.MaxTransferCount + 1))
    senderGen.zip(transferCountGen).map { case (sender, count) =>
      val transfers = List.tabulate(count)(_ => ParsedTransfer(recipientGen.next(), amountGen.next()))
      val fee = 100000 + count * 50000
      MassTransferTransaction.create(None, sender, transfers, System.currentTimeMillis, fee, Array.emptyByteArray)
    }.collect { case Right(tx) => tx }
  }

  val address: Iterator[Address] = Iterator.continually {
    val pk = Array.fill[Byte](TransactionParser.KeyLength)(random.nextInt(Byte.MaxValue).toByte)
    Address.fromPublicKey(pk)
  }

  def address(uniqNumber: Int): Iterator[Address] = Iterator.randomContinually(address.take(uniqNumber).toSeq)

  def address(limitUniqNumber: Option[Int]): Iterator[Address] = limitUniqNumber.map(address(_)).getOrElse(address)

}

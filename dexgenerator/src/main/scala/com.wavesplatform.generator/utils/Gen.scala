package com.wavesplatform.generator.utils

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.generator.utils.Implicits._
import scorex.account.{Address, PrivateKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction.transfer.MassTransferTransaction.ParsedTransfer
import scorex.transaction.transfer.{MassTransferTransaction, TransferTransactionV1}
import scorex.transaction.{Proofs, Transaction}

object Gen {
  private def random = ThreadLocalRandom.current

  def txs(minFee: Long, maxFee: Long, senderAccounts: Seq[PrivateKeyAccount], recipientGen: Iterator[Address]): Iterator[Transaction] = {
    val senderGen = Iterator.randomContinually(senderAccounts)
    val feeGen    = Iterator.continually(minFee + random.nextLong(maxFee - minFee))
    transfers(senderGen, recipientGen, feeGen)
      .zip(massTransfers(senderGen, recipientGen, feeGen))
      .flatMap { case (tt, mtt) => Iterator(mtt, tt) }
  }

  def transfers(senderGen: Iterator[PrivateKeyAccount], recipientGen: Iterator[Address], feeGen: Iterator[Long]): Iterator[Transaction] = {
    senderGen
      .zip(recipientGen)
      .zip(feeGen)
      .map {
        case ((src, dst), fee) =>
          TransferTransactionV1.selfSigned(None, src, dst, fee, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
      }
      .collect { case Right(x) => x }
  }

  def massTransfers(senderGen: Iterator[PrivateKeyAccount], recipientGen: Iterator[Address], amountGen: Iterator[Long]): Iterator[Transaction] = {
    val transferCountGen = Iterator.continually(random.nextInt(MassTransferTransaction.MaxTransferCount + 1))
    senderGen
      .zip(transferCountGen)
      .map {
        case (sender, count) =>
          val transfers = List.tabulate(count)(_ => ParsedTransfer(recipientGen.next(), amountGen.next()))
          val fee       = 100000 + count * 50000
          MassTransferTransaction.selfSigned(Proofs.Version, None, sender, transfers, System.currentTimeMillis, fee, Array.emptyByteArray)
      }
      .collect { case Right(tx) => tx }
  }

  val address: Iterator[Address] = Iterator.continually {
    val pk = Array.fill[Byte](KeyLength)(random.nextInt(Byte.MaxValue).toByte)
    Address.fromPublicKey(pk)
  }

  def address(uniqNumber: Int): Iterator[Address] = Iterator.randomContinually(address.take(uniqNumber).toSeq)

  def address(limitUniqNumber: Option[Int]): Iterator[Address] = limitUniqNumber.map(address(_)).getOrElse(address)

}

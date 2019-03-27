package com.wavesplatform.dexgen.utils

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.dexgen.utils.Implicits._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV1}
import scorex.crypto.signatures.Curve25519.KeyLength

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
    val now = System.currentTimeMillis()
    senderGen
      .zip(recipientGen)
      .zip(feeGen)
      .zipWithIndex
      .map {
        case (((src, dst), fee), i) =>
          TransferTransactionV1.selfSigned(Waves, src, dst, fee, now + i, Waves, fee, Array.emptyByteArray)
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
          MassTransferTransaction.selfSigned(Waves, sender, transfers, System.currentTimeMillis, fee, Array.emptyByteArray)
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

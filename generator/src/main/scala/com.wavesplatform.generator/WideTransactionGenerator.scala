package com.wavesplatform.generator

import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.Transaction
import scorex.transaction.assets.TransferTransaction

import scala.util.Random

class WideTransactionGenerator(val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  override def generate(count: Int): Seq[Transaction] = {
    val fee = 100000
    val seedSize = 32

    val sourcesAndDestinations = (1 to count).map { _ =>
      val src = Random.shuffle(accounts).head
      val pk = Array.fill[Byte](seedSize)(Random.nextInt(Byte.MaxValue).toByte)
      val dst = Address.fromPublicKey(pk)
      (src, dst)
    }

    sourcesAndDestinations.foldLeft(List.empty[TransferTransaction]) {
      case (txs, (src, dst)) =>
        val amount = Random.nextInt(fee) + 1
        val ts = System.currentTimeMillis()
        val maybeTransaction = TransferTransaction.create(None, src, dst, amount, ts, None, fee, Array.emptyByteArray)
        if (maybeTransaction.isRight) txs :+ maybeTransaction.right.get else txs
    }
  }
}

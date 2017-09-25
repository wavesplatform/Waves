package com.wavesplatform.generator.utils

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.generator.utils.Implicits._
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{Transaction, TransactionParser}

import scala.util.Random

object Gen {
  def txs(minFee: Long,
          maxFee: Long,
          accounts: Seq[PrivateKeyAccount]): Iterator[Transaction] = {
    def random = Random.javaRandomToRandom(ThreadLocalRandom.current)

    val senderGen = Iterator.randomContinually(accounts)

    val recipientGen = Iterator.continually {
      val pk = Array.fill[Byte](TransactionParser.KeyLength)(random.nextInt(Byte.MaxValue).toByte)
      Address.fromPublicKey(pk)
    }

    val feeGen = Iterator.continually {
      minFee + random.nextLong() % maxFee
    }

    senderGen.zip(recipientGen).zip(feeGen)
      .map {
        case ((src, dst), fee) =>
          TransferTransaction.create(None, src, dst, fee, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
      }
      .collect { case Right(x) => x }
  }
}

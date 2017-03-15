package com.wavesplatform.state2

import java.util

import cats._
import cats.implicits._
import cats.Monoid
import org.h2.mvstore.MVMap
import scorex.transaction.{GenesisTransaction, PaymentTransaction, SignedTransaction}

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit
}

class StateWriterImpl(p: JavaMapStorage) extends StateReaderImpl(p) with StateWriter {

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = {
    val txsDiff = blockDiff.txsDiff
    txsDiff.transactions.foreach { case (id, (h, tx)) =>
      p.transactions.put(id.arr, (h, tx.bytes))
    }

    txsDiff.portfolios.foreach { case (account, portfolioDiff) =>
      val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
      p.portfolios.put(account.bytes,
        (updatedPortfolio.balance,
          updatedPortfolio.effectiveBalance,
          updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
    }

    txsDiff.issuedAssets.foreach { case (id, assetInfo) =>
      p.assets.put(id.arr, (assetInfo.isReissuableOverride, assetInfo.totalVolumeOverride))
    }

    val affectedAccountsToIds = blockDiff.txsDiff.transactions.flatMap { case (id, (h, tx)) =>
      val senderBytes = tx match {
        case stx: SignedTransaction => stx.sender.bytes
        case ptx: PaymentTransaction => ptx.sender.bytes
        case gtx: GenesisTransaction => Array.empty[Byte]
        case _ => ???
      }

      val recipientBytes = tx match {
        case gtx: GenesisTransaction => gtx.recipient.bytes
        case ptx: PaymentTransaction => ptx.recipient.bytes
        case _ => ???
      }

      Map(senderBytes -> id, recipientBytes -> id)
    }


    affectedAccountsToIds.foreach { case (senderBytes, txId) =>
      val updatedTxIds: util.LinkedList[Array[Byte]] = Option(p.accountTransactionIds.get(senderBytes)) match {
        case Some(set) =>
          new util.LinkedList(set)
        case None =>
          new util.LinkedList[Array[Byte]]()
      }
      updatedTxIds.addFirst(txId.arr)

      p.accountTransactionIds.put(senderBytes, updatedTxIds)
    }
    p.setHeight(p.getHeight + blockDiff.heightDiff)
  }

}



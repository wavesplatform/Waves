package com.wavesplatform.state

import com.wavesplatform.block.Block
import com.wavesplatform.transaction.{ProvenTransaction, Transaction}

import java.util.concurrent.*

object ParSignatureChecker {

  private val rejectedHandler: RejectedExecutionHandler = (r: Runnable, executor: ThreadPoolExecutor) =>
    try executor.getQueue.put(r)
    catch {
      case ie: InterruptedException =>
        Thread.currentThread().interrupt()
        throw new RejectedExecutionException("Task submission interrupted", ie)
    }

  private val sigverify = new ThreadPoolExecutor(4, 8, 10, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable](100000), rejectedHandler)

  def checkTxSignatures(txs: Seq[Transaction], rideV6Activated: Boolean): Unit =
    txs.foreach {
      case tx: ProvenTransaction =>
        if (rideV6Activated) {
          sigverify.execute(() => tx.firstProofIsValidSignatureAfterV6)
        } else {
          sigverify.execute(() => tx.firstProofIsValidSignatureBeforeV6)
        }
      case _ =>
    }

  def checkBlockAndTxSignatures(block: Block, checkTransactionSignatures: Boolean, rideV6Activated: Boolean): Unit = {
    checkBlockSignature(block)
    if (checkTransactionSignatures && block.transactionData.nonEmpty) {
      checkTxSignatures(block.transactionData, rideV6Activated)
    }
  }

  def checkBlockSignature(block: Block): Unit =
    sigverify.execute(() => block.signatureValid())
}

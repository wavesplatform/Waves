package com.wavesplatform.state

import cats.syntax.parallel.*
import com.wavesplatform.block.Block
import com.wavesplatform.transaction.{ProvenTransaction, Transaction}
import com.wavesplatform.utils.Schedulers
import monix.eval.Task
import monix.execution.schedulers.SchedulerService

/** Executes signature verification in advance using parallel computation.
  * The result is cached in a lazy value: if precomputation completes in time,
  * the cached result is reused; otherwise, it is computed on demand.
  */
object ParSignatureChecker {
  implicit val sigverify: SchedulerService = Schedulers.fixedPool(4, "sigverify")

  def checkTxSignatures(txs: Seq[Transaction], rideV6Activated: Boolean): Unit =
    txs
      .parUnorderedTraverse {
        case tx: ProvenTransaction =>
          Task {
            if (rideV6Activated) {
              tx.firstProofIsValidSignatureAfterV6
            } else {
              tx.firstProofIsValidSignatureBeforeV6
            }
          }.void
        case _ => Task.unit
      }
      .executeOn(sigverify)
      .runAsyncAndForget

  def checkBlockAndTxSignatures(block: Block, checkTxSignatures: Boolean, rideV6Activated: Boolean): Unit = {
    val verifiedObjects: Seq[Any] = (block +: block.transactionData)
    verifiedObjects
      .parTraverse {
        case tx: ProvenTransaction if checkTxSignatures =>
          Task {
            if (rideV6Activated) {
              tx.firstProofIsValidSignatureAfterV6
            } else {
              tx.firstProofIsValidSignatureBeforeV6
            }
          }.void
        case b: Block => Task(b.signatureValid()).void
        case _        => Task.unit
      }
      .executeOn(sigverify)
      .runAsyncAndForget
  }

  def checkBlockSignature(block: Block): Unit =
    Task(block.signatureValid()).executeOn(sigverify).runAsyncAndForget
}

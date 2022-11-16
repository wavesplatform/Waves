package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.blockchain.BlockchainProcessor.ProcessResult
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.ride.RideScript
import com.wavesplatform.state.Height
import com.wavesplatform.storage.DataKey
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json.JsObject

import scala.util.chaining.scalaUtilChainingOps

trait Processor {

  /** @return
    *   None if has no block at this height
    */
  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean]
  def removeFrom(height: Height): Unit
  def process(event: BlockchainUpdated): Unit
  def runScripts(forceAll: Boolean = false): Unit
}

class BlockchainProcessor(
    blockchainStorage: SharedBlockchainData[Int],
    scripts: Vector[RideScript]
) extends Processor
    with ScorexLogging {
  private val allScriptIndices              = scripts.indices.toSet
  @volatile private var curr: ProcessResult = ProcessResult()

  override def process(event: BlockchainUpdated): Unit = {
    val height = Height(event.height)
    curr = event.update match {
      case Update.Empty              => curr // Ignore
      case Update.Append(append)     => process(height, append)
      case Update.Rollback(rollback) => process(height, rollback)
    }

    // Update this in the end, because a service could be suddenly turned off and we won't know, that we should re-read this block
    blockchainStorage.blockHeaders.update(event)
  }

  private def process(h: Height, append: BlockchainUpdated.Append): ProcessResult = {
    // TODO the height will be eventually > if this is a rollback
    // Almost all scripts use the height
    val withUpdatedHeight = curr.copy(newHeight = h) // TODO

    val txs = append.body match {
      // PBBlocks.vanilla(block.getBlock.getHeader)
      case Body.Block(block)           => block.getBlock.transactions
      case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
      case Body.Empty                  => Seq.empty
    }

    val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
    val txsView     = txs.view.map(_.transaction)
    withUpdatedHeight
      .pipe(stateUpdate.flatMap(_.assets).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.assets.append(h, x))
      })
      .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.portfolios.append(h, x))
      })
      .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.portfolios.append(h, x))
      })
      .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.data.append(h, x))
      })
      .pipe(
        txsView
          .flatMap {
            case Transaction.WavesTransaction(tx) =>
              tx.data match {
                case Data.SetScript(txData) => (tx.senderPublicKey.toPublicKey, txData.script).some
                case _                      => none
              }
            case _ => none
          }
          .foldLeft(_) { case (r, (pk, script)) =>
            r.withAppendResult(blockchainStorage.accountScripts.append(h, pk, script))
          }
      )
      .pipe(
        txsView
          .flatMap {
            case Transaction.WavesTransaction(tx) =>
              tx.data match {
                case Data.CreateAlias(txData) => (txData.alias, tx.senderPublicKey.toPublicKey).some
                case _                        => none
              }
            case _ => none
          }
          .foldLeft(_) { case (r, (alias, pk)) =>
            r.withAppendResult(blockchainStorage.aliases.append(h, alias, pk))
          }
      )
      .pipe(append.transactionIds.view.zip(txs).foldLeft(_) { case (r, (txId, tx)) =>
        r.withAppendResult(blockchainStorage.transactions.append(h, txId, tx))
      })
  }

  private def process(h: Height, rollback: BlockchainUpdated.Rollback): ProcessResult = {
    // TODO the height will be eventually > if this is a rollback
    // Almost all scripts use the height
    val withUpdatedHeight = curr.copy(newHeight = h) // TODO

    val stateUpdate = rollback.getRollbackStateUpdate
    withUpdatedHeight
      .pipe(stateUpdate.assets.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.assets.rollback(h, x))
      })
      /* TODO:
      .pipe(stateUpdate.aliases.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.aliases.rollback(h, x))
      })*/
      .pipe(stateUpdate.balances.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.portfolios.rollback(h, x))
      })
      .pipe(stateUpdate.leasingForAddress.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.portfolios.rollback(h, x))
      })
      .pipe(stateUpdate.dataEntries.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.data.rollback(h, x))
      })
    /* TODO:
        .pipe(stateUpdate.accountScripts.foldLeft(_) { case (r, x) =>
          r.withRollbackResult(blockchainStorage.accountScripts.rollback(h, x))
        })
        // TODO Remove?
        .pipe(append.transactionIds.view.zip(txs).foldLeft(_) { case (r, txId) =>
          r.withRollbackResult(blockchainStorage.transactions.rollback(h, txId)
        })*/

  }

  override def runScripts(forceAll: Boolean = false): Unit = {
    val height = curr.newHeight
    if (curr.uncertainKeys.nonEmpty) {
      log.debug(s"Getting data for keys: ${curr.uncertainKeys.toVector.map(_.toString).sorted.mkString(", ")}")
      curr.uncertainKeys.foreach {
        _.reload(height)
      }
    }

    val affectedScripts = if (forceAll) allScriptIndices else curr.affectedScripts

    // log.info(
    //   s"==> $h >= $lastHeightAtStart, started: $started, affectedScripts: ${curr.affectedScripts}, batchedEvents for heights: {${batchedEvents
    //     .map(_.getUpdate.height)
    //     .mkString(", ")}}"
    // )

    if (affectedScripts.isEmpty) log.debug(s"[$height] Not updated")
    else {
      log.debug(s"[$height] Updated for: ${affectedScripts.mkString(", ")}")
      runScripts(height, affectedScripts)
    }

    curr = ProcessResult()
  }

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = blockchainStorage.blockHeaders.getLocal(height).map(_.id() == id)

  override def removeFrom(height: Height): Unit = blockchainStorage.blockHeaders.removeFrom(height)

  private def runScripts(height: Int, updated: Set[Int]): Unit = updated.foreach { index =>
    val apiResult = scripts(index).run()
    log.info(s"[$height, $index] apiResult: ${apiResult.value("result").as[JsObject].value("value")}")
  }
}

object BlockchainProcessor {
  // TODO don't calculate affectedScripts if all scripts are affected
  private case class ProcessResult(
      /*allScripts: Set[Int], */ newHeight: Int = 0,
      affectedScripts: Set[Int] = Set.empty,
      uncertainKeys: Set[DataKey] = Set.empty
  ) {
    def withAppendResult(x: AppendResult[Int]): ProcessResult =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = x.mayBeChangedKey.foldLeft(uncertainKeys)(_ - _)
      )

    def withRollbackResult(x: RollbackResult[Int]): ProcessResult =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = uncertainKeys ++ x.mayBeUncertainKey
      )
  }
}

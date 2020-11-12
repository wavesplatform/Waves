package com.wavesplatform.events.repo

import scala.util.Try

import com.wavesplatform.events._
import monix.reactive.Observable

object UpdatesRepo {
  trait Read {
    def height: Try[Int]

    def updateForHeight(height: Int): Try[Option[BlockAppended]]

    // inclusive from both sides
    def updatesRange(from: Int, to: Int): Observable[BlockAppended]
  }

  trait Write {
    //  def dropLiquidState(afterId: Option[ByteStr] = None): Unit

    def appendMicroBlock(microBlockAppended: MicroBlockAppended): Try[Unit]

    def appendBlock(blockAppended: BlockAppended): Try[Unit]

    def rollback(rollback: RollbackCompleted): Try[Unit]

    def rollbackMicroBlock(microBlockRollback: MicroBlockRollbackCompleted): Try[Unit]
  }

  trait Stream {
    // inclusive
    def stream(from: Int): Observable[BlockchainUpdated]
  }
}

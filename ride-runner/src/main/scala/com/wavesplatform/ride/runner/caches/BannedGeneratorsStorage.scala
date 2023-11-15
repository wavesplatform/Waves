package com.wavesplatform.ride.runner.caches

import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.ride.runner.caches.disk.BannedGeneratorsDiskCache
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}

class BannedGeneratorsStorage(diskCache: BannedGeneratorsDiskCache) extends OptimisticLockable with ScorexLogging {
  private var bannedGenerators: Vector[BannedGenerator] = Vector.empty

  def load(from: Height)(implicit ctx: ReadOnly): Unit = writeLock {
    bannedGenerators = diskCache.getFrom(from)
  }

  def getHeights(generator: Address): Seq[Int] = bannedGenerators.collect { case x if x.address == generator => x.height }

  def update(event: BlockchainUpdated)(implicit ctx: ReadWrite): Unit = writeLock {
    event.update match {
      case Update.Empty => // Ignore
      case Update.Append(append) =>
        append.body match {
          case Body.Block(block) =>
            val maliciousGenerator = block.getBlock.getHeader.challengedHeader.map(_.generator)
            if (maliciousGenerator.nonEmpty) {
              log.debug(s"Added at ${event.height}")
            }
          case _ => // Ignore
        }

      case _: Update.Rollback =>
        val toHeight = Height(event.height)
        bannedGenerators
          .find(_.height >= toHeight)
          .foreach { x =>
            val firstHeightToRemove = x.height
            log.debug(s"Remove from $firstHeightToRemove")
            removeFrom(firstHeightToRemove)
          }
    }
  }

  def removeFrom(height: Height)(implicit ctx: ReadWrite): Unit = writeLock {
    bannedGenerators = bannedGenerators.takeWhile(_.height < height)
    diskCache.removeFrom(height)
  }
}

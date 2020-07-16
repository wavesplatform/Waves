package com.wavesplatform.events

import com.wavesplatform.extensions.{Context, Extension}
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.utils.{ScorexLogging, forceStopApplication}
import com.wavesplatform.events.protobuf.{BlockchainUpdated => PBBlockchainUpdated}

import scala.concurrent.Future

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging {
  import monix.execution.Scheduler.Implicits.global

  private[this] val settings = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")

  override def start(): Unit = {
      log.info("BlockchainUpdates extension starting")
  }

  override def shutdown(): Future[Unit] = Future {
    log.info("BlockchainUpdates extension shitting down")
  }
}

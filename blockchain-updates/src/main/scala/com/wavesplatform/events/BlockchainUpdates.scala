package com.wavesplatform.events

import com.wavesplatform.extensions.{Context, Extension}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.state.BlockchainUpdated
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.reactive.Observer
import org.apache.kafka.clients.producer.KafkaProducer

import com.wavesplatform.events.kafka.{createProducer, createProducerRecord}

import scala.concurrent.Future

class BlockchainUpdates(context: Context) extends Extension with ScorexLogging {
  import monix.execution.Scheduler.Implicits.global

  private val settings = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")

  var maybeProducer: Option[KafkaProducer[Int, BlockchainUpdated]] = None

  override def start(): Unit = {
    context.blockchainUpdated foreach { blockchainUpdated =>
      maybeProducer = Some(createProducer(settings))
      maybeProducer foreach { producer =>
        log.info("Starting sending blockchain updates to Kafka")
        blockchainUpdated.subscribe(new Observer.Sync[BlockchainUpdated] {
          override def onNext(elem: BlockchainUpdated): Ack = {
            producer.send(createProducerRecord(settings.topic, elem))
            Continue
          }
          override def onError(ex: Throwable): Unit = {
            log.error("Error sending blockchain updates", ex)
            // @todo should it be synchronous?
            shutdown()
          }
          override def onComplete(): Unit = {
            log.info("Blockchain updates Observable complete")
            shutdown()
          }
        })
      }
    }
  }

  override def shutdown(): Future[Unit] = Future {
    log.info("Shutting down blockchain updates sending")
    maybeProducer foreach (_.close())
  }
}

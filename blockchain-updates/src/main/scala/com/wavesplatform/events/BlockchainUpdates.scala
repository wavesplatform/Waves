package com.wavesplatform.events

import java.time.{Duration => JDuration}
import java.util
import java.util.Properties

import com.wavesplatform.extensions.{Context, Extension}
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.state.BlockchainUpdated
import com.wavesplatform.utils.{ScorexLogging, forceStopApplication}
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.reactive.Observer
import org.apache.kafka.clients.producer.{KafkaProducer, RecordMetadata}
import com.wavesplatform.events.kafka.{createProducer, createProducerRecord}
import org.apache.kafka.clients.consumer.{ConsumerConfig, KafkaConsumer}
import org.apache.kafka.common.serialization.Deserializer
import com.wavesplatform.events.protobuf.PBBlockchainUpdated
import org.apache.kafka.common.TopicPartition

import scala.concurrent.Future
import scala.concurrent.duration._

class BlockchainUpdates(context: Context) extends Extension with ScorexLogging {
  import monix.execution.Scheduler.Implicits.global

  private[this] val settings = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")

  private[this] var maybeProducer: Option[KafkaProducer[Int, BlockchainUpdated]] = None

  private def getLastHeight(timeout: Duration = 10.seconds): Int = {
    import scala.collection.JavaConverters._

    val props = new Properties()
    props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, settings.bootstrapServers)
    props.put(ConsumerConfig.GROUP_ID_CONFIG, "admin")
    props.put(ConsumerConfig.CLIENT_ID_CONFIG, settings.clientId)
    props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false")
    props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000")
    props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "1")

    val consumer = new KafkaConsumer[Unit, Int](
      props,
      new Deserializer[Unit] {
        override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
        override def deserialize(topic: String, data: Array[Byte]): Unit           = {}
        override def close(): Unit                                                 = {}
      },
      new Deserializer[Int] { // height of last event
        override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
        override def deserialize(topic: String, data: Array[Byte]): Int =
          PBBlockchainUpdated.parseFrom(data).height
        override def close(): Unit = {}
      }
    )

    val partition = consumer.partitionsFor(settings.topic).asScala.head
    val tp = new TopicPartition(settings.topic, partition.partition)

    consumer.assign(util.Arrays.asList(tp))
    consumer.seek(tp, consumer.endOffsets(util.Arrays.asList(tp)).asScala.apply(tp) - 1)

    val records = consumer.poll(JDuration.ofMillis(timeout.toMillis))

    if (records.isEmpty) 0
    else records.records(tp).asScala.last.value
  }

  private[this] def startupCheck(): Unit = {
    // if kafkaHeight <= (blockchainHeight + 1) — rollback node with event sending to Kafka. If rollback fails — fail
    // if kafkaHeight > (blockchainHeight + 1) — fail. This should not happen
    // if kafka is empty, but blockchain is further than genesis block — fail
    // if both kafka and blockchain are empty — OK

    // Idea for better checks: maintain Kafka view of blocks with signatures and check for (and recover from) forks.
    // The view can be maintained via transaction writes

    val blockchainHeight = context.blockchain.height
    val kafkaHeight      = getLastHeight()

    if (kafkaHeight == 0 && blockchainHeight > 1)
      throw new IllegalStateException("No events in Kafka, but blockchain is neither empty nor on genesis block.")

    if (kafkaHeight > blockchainHeight + 1)
      throw new IllegalStateException(s"""Node is behind kafka. Kafka is at $kafkaHeight, while node is at $blockchainHeight.
                                         |This should never happen. Manual correction of even full system restart might be necessary.""".stripMargin)

    if (kafkaHeight != 0) {
      val heightToRollbackTo = Math.max(kafkaHeight - 1, 1)
      val sigToRollback = context.blockchain
        .blockHeaderAndSize(heightToRollbackTo)
        .map(_._1.signerData.signature)
        .get // guaranteed not to fail by previous checks on heights

      log.info(s"Kafka is at $kafkaHeight, while node is at $blockchainHeight. Rolling node back to $heightToRollbackTo")
      context.blockchain.removeAfter(sigToRollback) match {
        case Right(_) =>
        case Left(_) =>
          throw new IllegalStateException(s"Unable to rollback Node to Kafka state. Kafka is at $kafkaHeight, while node is at $blockchainHeight.")
      }
    }
  }

  override def start(): Unit = {
    context.blockchainUpdated foreach { blockchainUpdated =>
      maybeProducer = Some(createProducer(settings))
      maybeProducer foreach { producer =>
        log.info("Performing startup node/Kafka consistency check...")

        blockchainUpdated.subscribe(new Observer.Sync[BlockchainUpdated] {
          override def onNext(elem: BlockchainUpdated): Ack = {
            producer.send(
              createProducerRecord(settings.topic, elem),
              (_: RecordMetadata, exception: Exception) =>
                if (exception != null) {
                  log.error("Error sending blockchain updates", exception)
                  forceStopApplication()
              }
            )
            Continue
          }
          override def onError(ex: Throwable): Unit = {
            log.error("Error sending blockchain updates", ex)
            forceStopApplication()
          }
          override def onComplete(): Unit = {
            log.error("Blockchain updates Observable complete")
            forceStopApplication() // this should never happen, but just in case, explicit stop.
          }
        })

        // startupCheck is after subscription, so that if the check makes a rollback, it would be handled
        startupCheck()
        log.info("Starting sending blockchain updates to Kafka")
      }
    }
  }

  override def shutdown(): Future[Unit] = Future {
    log.info("Shutting down blockchain updates sending")
    maybeProducer foreach (_.close())
  }
}

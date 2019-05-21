package com.wavesplatform.events

import java.time.{Duration => JDuration}
import java.util
import java.util.Properties

import com.wavesplatform.extensions.{Context, Extension}
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.state.{BlockchainUpdated, MicroBlockRollbackCompleted}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Ack
import monix.execution.Ack.Continue
import monix.reactive.Observer
import org.apache.kafka.clients.producer.KafkaProducer
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

  private[this] def getLastHeight(timeout: Duration = 10.seconds): Option[Int] = {
    import scala.collection.JavaConverters._

    val props = new Properties()
    props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, settings.bootstrapServers)
    props.put(ConsumerConfig.GROUP_ID_CONFIG, "admin")
    props.put(ConsumerConfig.CLIENT_ID_CONFIG, settings.clientId)
    props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false")
    props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000")
    props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "1")

    // read height and whether last event in Kafka is MicroBlock or MicroBlockRollback
    val consumer = new KafkaConsumer[Unit, Int](
      props,
      new Deserializer[Unit] {
        override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
        override def deserialize(topic: String, data: Array[Byte]): Unit           = {}
        override def close(): Unit                                                 = {}
      },
      new Deserializer[Int] {
        override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
        override def deserialize(topic: String, data: Array[Byte]): Int =
          PBBlockchainUpdated.parseFrom(data).height
        override def close(): Unit = {}
      }
    )

    try {
      consumer.partitionsFor(settings.topic).asScala.headOption.flatMap { partition =>
        val tp = new TopicPartition(settings.topic, partition.partition)
        consumer.assign(util.Arrays.asList(tp))
        consumer.seek(tp, consumer.endOffsets(util.Arrays.asList(tp)).asScala.apply(tp) - 1)

        val records = consumer.poll(JDuration.ofMillis(timeout.toMillis))

        if (records.isEmpty) None
        else records.records(tp).asScala.lastOption.map(_.value)
      }
    } catch { case _: Throwable => None }
  }

  @throws[IllegalStateException]("if events in Kafka are different from node state and it's impossible to recover")
  private[this] def startupCheck(): Unit = {
    // if kafkaHeight <= blockchainHeight — rollback node with event sending to Kafka. If rollback fails — fail
    // if kafkaHeight > blockchainHeight — fail. This should not happen

    // For later (maybe):, blockchain consistency can be checked better using Kafka view of blocks with signatures

    val blockchainHeight = context.blockchain.height
    getLastHeight() match {
      case Some(kafkaHeight) =>
        if (kafkaHeight <= blockchainHeight) {
          /*
          Always rollback node on startup at least 1 block, since node loses ngState
          and it's hard to tell whether the block in Kafka is full or not.

          Rollback node to (kafkaHeight - 1) to be sure, since previous block is guaranteed to be solid.
           */

          try {
            val heightToRollback = Math.max(kafkaHeight - 1, 1)
            val sigToRollback    = context.blockchain.blockHeaderAndSize(heightToRollback).get._1.signerData.signature
            log.info(s"Kafka is at $kafkaHeight, while node is at $blockchainHeight. Rolling node back to $heightToRollback")
            context.blockchain.removeAfter(sigToRollback) // this already sends Rollback event
          } catch {
            case _: Throwable =>
              throw new IllegalStateException(
                s"Unable to rollback Node to Kafka state. Kafka is at $kafkaHeight, while node is at $blockchainHeight.")
          }
        } else
          throw new IllegalStateException(s"""Node is behind kafka. Kafka is at $kafkaHeight, while node is at $blockchainHeight.
               |This should never happen. Manual correction of even full system restart might be necessary.""".stripMargin)

      // Genesis block gets applied before extension starts, so it's possible to have no events in Kafka, but blockchainHeight == 1.
      case None if blockchainHeight > 1 =>
        throw new IllegalStateException("No events in Kafka, but blockchain is neither empty nor on genesis block.")

      case _ => ()
    }
  }

  override def start(): Unit = {
    context.blockchainUpdated foreach { blockchainUpdated =>
      maybeProducer = Some(createProducer(settings))
      maybeProducer foreach { producer =>
        log.info("Performing startup node/Kafka consistency check...")
        startupCheck()

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

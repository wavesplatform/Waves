package com.wavesplatform.matcher.queue

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import akka.kafka._
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.pattern.ask
import akka.stream.scaladsl.{Keep, RestartSource, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import com.wavesplatform.matcher.queue.KafkaMatcherQueue.Settings
import com.wavesplatform.utils.ScorexLogging
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContextExecutor, Future, Promise}

class KafkaMatcherQueue(settings: Settings)(implicit mat: ActorMaterializer) extends MatcherQueue with ScorexLogging {
  private implicit val dispatcher: ExecutionContextExecutor = mat.system.dispatcher

  private val duringShutdown = new AtomicBoolean(false)

  private val deserializer = new Deserializer[QueueEvent] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def deserialize(topic: String, data: Array[Byte]): QueueEvent          = QueueEvent.fromBytes(data)
    override def close(): Unit                                                      = {}
  }

  private val serializer = new Serializer[QueueEvent] {
    override def configure(configs: java.util.Map[String, _], isKey: Boolean): Unit = {}
    override def serialize(topic: String, data: QueueEvent): Array[Byte]            = QueueEvent.toBytes(data)
    override def close(): Unit                                                      = {}
  }

  private val producerControl = new AtomicReference[() => Unit](() => ())
  private val producerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.producer")
    ProducerSettings(config, new ByteArraySerializer, serializer)
  }

  private def newProducer =
    Source
      .queue[(QueueEvent, Promise[QueueEventWithMeta])](settings.producer.bufferSize, OverflowStrategy.backpressure)
      .mapMaterializedValue { x =>
        producerControl.set(() => x.complete())
        x
      }
      .map {
        case (payload, p) =>
          ProducerMessage.single(new ProducerRecord[Array[Byte], QueueEvent](settings.topic, payload.assetPair.bytes, payload), passThrough = p)
      }
      .via(Producer.flexiFlow(producerSettings))
      .map {
        case ProducerMessage.Result(meta, ProducerMessage.Message(msg, passThrough)) =>
          passThrough.success(QueueEventWithMeta(meta.offset(), meta.timestamp(), msg.value()))
        case ProducerMessage.MultiResult(parts, passThrough) => throw new RuntimeException(s"MultiResult(parts=$parts, passThrough=$passThrough)")
        case ProducerMessage.PassThroughResult(passThrough)  => throw new RuntimeException(s"PassThroughResult(passThrough=$passThrough)")
      }
      .toMat(Sink.ignore)(Keep.left)
      .run()

  private var producer = newProducer
  watchProducer()

  private def watchProducer(): Unit = {
    producer.watchCompletion().onComplete { _ =>
      if (!duringShutdown.get()) {
        producer = newProducer
        watchProducer()
      }
    }
  }

  private val consumerControl = new AtomicReference[Consumer.Control](Consumer.NoopControl)
  private val consumerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.consumer")
    ConsumerSettings(config, new ByteArrayDeserializer, deserializer).withClientId("consumer")
  }

  private val metadataConsumer = mat.system.actorOf(
    KafkaConsumerActor.props(consumerSettings.withClientId("meta-consumer")),
    "meta-consumer"
  )

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Unit): Unit = {
    log.info(s"Start consuming from $fromOffset")
    var currentOffset  = fromOffset // Store locally to know a previous processed offset when the source is restarted
    val topicPartition = new TopicPartition(settings.topic, 0)

    RestartSource
      .onFailuresWithBackoff(
        minBackoff = settings.consumer.minBackoff,
        maxBackoff = settings.consumer.maxBackoff,
        randomFactor = 0.2,
        maxRestarts = -1
      ) { () =>
        Consumer
          .plainSource(consumerSettings, Subscriptions.assignmentWithOffset(topicPartition -> currentOffset))
          .mapMaterializedValue(consumerControl.set)
          .buffer(settings.consumer.bufferSize, OverflowStrategy.backpressure)
          .map { msg =>
            // We can do it in parallel, because we're just applying verified events
            val req = QueueEventWithMeta(msg.offset(), msg.timestamp(), msg.value())
            process(req)
            currentOffset = msg.offset() // Messages are received one-by-one, e.g. offsets: 1, 2, 3, ...
          }
      }
      .runWith(Sink.ignore)
  }

  override def storeEvent(event: QueueEvent): Future[QueueEventWithMeta] = {
    val p = Promise[QueueEventWithMeta]()
    producer.offer((event, p))
    p.future
  }

  override def lastEventOffset: Future[QueueEventWithMeta.Offset] = {
    implicit val timeout: Timeout = Timeout(consumerSettings.metadataRequestTimeout)

    // We need to list topic first, see the EndOffsets documentation
    (metadataConsumer ? Metadata.ListTopics).mapTo[Metadata.Topics].flatMap { topics =>
      topics.response.getOrElse(Map.empty).get(settings.topic) match {
        case None => Future.successful(-1L)
        case Some(partitions) =>
          if (partitions.size != 1) throw new IllegalStateException(s"DEX can work only with one partition, given: $partitions")
          val topicPartition = new TopicPartition(settings.topic, partitions.head.partition())
          (metadataConsumer ? Metadata.GetEndOffsets(Set(topicPartition)))
            .mapTo[Metadata.EndOffsets]
            .map { r =>
              // -1 because by contract lastEventOffset must return -1 if there is no topic or it is empty
              // also see https://github.com/apache/kafka/blob/trunk/clients/src/main/java/org/apache/kafka/clients/consumer/KafkaConsumer.java#L2025
              r.response
                .getOrElse(Map.empty)
                .getOrElse(topicPartition, throw new IllegalStateException(s"Unexpected behaviour, no info for $topicPartition: $r")) - 1
            }
      }
    }
  }

  override def close(timeout: FiniteDuration): Unit = {
    duringShutdown.set(true)
    mat.system.stop(metadataConsumer)
    val stoppingConsumer = consumerControl.get().shutdown()
    producer.complete()
    Await.result(producer.watchCompletion(), timeout)
    Await.result(stoppingConsumer, timeout)
  }

}

object KafkaMatcherQueue {
  case class Settings(topic: String, consumer: ConsumerSettings, producer: ProducerSettings)
  case class ConsumerSettings(bufferSize: Int, minBackoff: FiniteDuration, maxBackoff: FiniteDuration)
  case class ProducerSettings(bufferSize: Int)
}

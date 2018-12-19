package com.wavesplatform.matcher.queue

import java.util.concurrent.atomic.AtomicReference

import akka.kafka._
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.scaladsl.{Keep, RestartSource, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.wavesplatform.matcher.queue.KafkaMatcherQueue.Settings
import com.wavesplatform.utils.ScorexLogging
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.{Deserializer, Serializer, StringDeserializer, StringSerializer}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContextExecutor, Future, Promise}

class KafkaMatcherQueue(settings: Settings)(implicit mat: ActorMaterializer) extends MatcherQueue with ScorexLogging {
  private implicit val dispatcher: ExecutionContextExecutor = mat.system.dispatcher

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

  private val consumerControl = new AtomicReference[Consumer.Control](Consumer.NoopControl)

  private val consumerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.consumer")
    ConsumerSettings(config, new StringDeserializer, deserializer)
  }

  private val producerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.producer")
    ProducerSettings(config, new StringSerializer, serializer)
  }

  private val producer = Source
    .queue[(QueueEvent, Promise[QueueEventWithMeta.Offset])](settings.producer.bufferSize, OverflowStrategy.backpressure)
    .map {
      case (payload, p) =>
        ProducerMessage.single(new ProducerRecord[String, QueueEvent](settings.topic, null, payload), passThrough = p)
    }
    .via(Producer.flexiFlow(producerSettings))
    .map {
      case ProducerMessage.Result(meta, ProducerMessage.Message(_, passThrough)) => passThrough.success(meta.offset())
      case ProducerMessage.MultiResult(parts, passThrough)                       => throw new RuntimeException(s"MultiResult(parts=$parts, passThrough=$passThrough)")
      case ProducerMessage.PassThroughResult(passThrough)                        => throw new RuntimeException(s"PassThroughResult(passThrough=$passThrough)")
    }
    .toMat(Sink.ignore)(Keep.left)
    .run()

  override def startConsume(fromOffset: QueueEventWithMeta.Offset, process: QueueEventWithMeta => Future[Unit]): Unit = {
    log.info(s"Start consuming from $fromOffset")
    RestartSource
      .onFailuresWithBackoff(
        minBackoff = settings.consumer.minBackoff,
        maxBackoff = settings.consumer.maxBackoff,
        randomFactor = 0.2,
        maxRestarts = -1
      ) { () =>
        Consumer
          .committableSource(consumerSettings, Subscriptions.assignmentWithOffset(new TopicPartition(settings.topic, 0) -> fromOffset))
          .mapMaterializedValue(consumerControl.set)
          .buffer(settings.consumer.bufferSize, OverflowStrategy.backpressure)
          .mapAsync(1) { msg =>
            val req = QueueEventWithMeta(msg.record.offset(), msg.record.timestamp(), msg.record.value())
            process(req)
              .recoverWith {
                case e =>
                  log.error(s"Matcher: Failed to process event at ${msg.record.offset()} offset: ${msg.record}")
                  Future.failed(e)
              }
              .map(_ => msg.committableOffset)
          }
          .batch(max = settings.consumer.bufferSize, ConsumerMessage.CommittableOffsetBatch(_))(_.updated(_))
          .mapAsync(2)(_.commitScaladsl())
      }
      .runWith(Sink.ignore)
  }

  override def storeEvent(event: QueueEvent): Future[QueueEventWithMeta.Offset] = {
    val p = Promise[QueueEventWithMeta.Offset]()
    producer.offer((event, p))
    p.future
  }

  override def close(timeout: FiniteDuration): Unit = {
    producer.complete()
    Await.result(producer.watchCompletion(), timeout)
    Await.result(consumerControl.get().shutdown(), timeout)
  }

}

object KafkaMatcherQueue {
  case class Settings(topic: String, consumer: ConsumerSettings, producer: ProducerSettings)
  case class ConsumerSettings(bufferSize: Int, minBackoff: FiniteDuration, maxBackoff: FiniteDuration)
  case class ProducerSettings(bufferSize: Int)
}

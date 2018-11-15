package com.wavesplatform.matcher.queue

import akka.kafka._
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.scaladsl.{Keep, Sink, Source}
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

  private var consumer = Option.empty[Consumer.Control]

  private val consumerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.consumer")
    ConsumerSettings(config, new StringDeserializer, deserializer)
  }

  private val producerSettings = {
    val config = mat.system.settings.config.getConfig("akka.kafka.producer")
    ProducerSettings(config, new StringSerializer, serializer)
  }

  private val commandProducer = Source
    .queue[(QueueEvent, Promise[QueueEventWithMeta.Offset])](1, OverflowStrategy.backpressure)
    .map {
      case (payload, p) =>
        ProducerMessage.single(new ProducerRecord(settings.topic, "assetPair", payload), passThrough = p)
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
    consumer = Some {
      Consumer
        .committableSource(consumerSettings, Subscriptions.assignmentWithOffset(new TopicPartition(settings.topic, 0) -> fromOffset))
        .buffer(settings.consumerBufferSize, OverflowStrategy.dropTail)
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
        .batch(max = settings.consumerBufferSize, ConsumerMessage.CommittableOffsetBatch(_))(_.updated(_))
        .mapAsync(5)(_.commitScaladsl())
        .toMat(Sink.seq)(Keep.left)
        .run()
    }
  }

  override def storeEvent(event: QueueEvent): Future[QueueEventWithMeta.Offset] = {
    val p = Promise[QueueEventWithMeta.Offset]()
    commandProducer.offer((event, p))
    p.future
  }

  override def close(timeout: FiniteDuration): Unit = {
    commandProducer.complete()
    Await.result(commandProducer.watchCompletion(), timeout)
    consumer.foreach(x => Await.result(x.shutdown(), timeout))
  }

}

object KafkaMatcherQueue {
  case class Settings(topic: String, consumerBufferSize: Int)
}

package tools

import java.io.File
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import akka.kafka.scaladsl.Consumer
import akka.kafka.{ConsumerSettings, KafkaConsumerActor, Metadata, Subscriptions}
import akka.pattern.ask
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.queue.KafkaMatcherQueue.eventDeserializer
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.ByteArrayDeserializer

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

object KafkaTopicInfo extends App {
  implicit val system: ActorSystem = ActorSystem()

  val configFile = new File(args(1))
  val topic      = args(2)
  val partition  = args(3).toInt
  val from       = args(4).toLong
  val max        = args(5).toInt

  try {
    implicit val timeout: Timeout = Timeout(5.seconds)

    val consumerControl = new AtomicReference[Consumer.Control](Consumer.NoopControl)

    val config = ConfigFactory
      .parseFile(configFile)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
      .getConfig("akka.kafka.consumer")

    val consumerSettings = ConsumerSettings(config, new ByteArrayDeserializer, eventDeserializer).withClientId("consumer").withGroupId("0")

    val metadataConsumer = system.actorOf(
      KafkaConsumerActor.props(consumerSettings),
      "meta-consumer"
    )

    val topicPartition = new TopicPartition(topic, partition)
    val r = Await.result(metadataConsumer
                           .ask(Metadata.GetEndOffsets(Set(topicPartition)))
                           .mapTo[Metadata.EndOffsets],
                         timeout.duration)
    println(s"Meta: $r")

    implicit val mat: ActorMaterializer = ActorMaterializer()

    val lock = new CountDownLatch(max)
    val consumer = Consumer
      .plainSource(consumerSettings, Subscriptions.assignmentWithOffset(topicPartition -> from))
      .mapMaterializedValue(consumerControl.set)
      .buffer(100, OverflowStrategy.backpressure)
      .map { msg =>
        QueueEventWithMeta(msg.offset(), msg.timestamp(), msg.value())
      }

    consumer.runForeach { x =>
      println(x)
      lock.countDown()
    }

    lock.await()
  } finally {
    system.terminate()
  }
}

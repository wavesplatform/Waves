package tools

import java.io.File
import java.util.concurrent.CountDownLatch

import akka.actor.ActorSystem
import akka.kafka.Metadata.PartitionsFor
import akka.kafka.scaladsl.Consumer
import akka.kafka.{ConsumerSettings, KafkaConsumerActor, Metadata, Subscriptions}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.queue.KafkaMatcherQueue.eventDeserializer
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.ByteArrayDeserializer

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

object KafkaTopicInfo extends App {
  implicit val system: ActorSystem = ActorSystem()

  val configFile = new File(args(0))
  val topic      = args(1)
  val from       = args(2).toLong
  val max        = args(3).toInt

  println(s"""configFile: ${configFile.getAbsolutePath}
       |topic: $topic
       |from: $from
       |max: $max""".stripMargin)

  try {
    implicit val timeout: Timeout = Timeout(5.seconds)

    val config = ConfigFactory
      .parseFile(configFile)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
      .getConfig("akka.kafka.consumer")

    val consumerSettings =
      ConsumerSettings(config, new ByteArrayDeserializer, eventDeserializer)
        .withClientId("consumer")
        .withGroupId("kafka-topics-info")
        .withProperties(
          Map(
            ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG -> "false",
            ConsumerConfig.AUTO_OFFSET_RESET_CONFIG  -> "earliest"
          ))

    val metadataConsumer = system.actorOf(
      KafkaConsumerActor.props(consumerSettings),
      "meta-consumer"
    )

    {
      val partitions = Await.result(metadataConsumer.ask(Metadata.GetPartitionsFor(topic)).mapTo[PartitionsFor], timeout.duration)
      println(s"Partitions: ${partitions.response.toOption}")
    }

    val topicPartition = new TopicPartition(topic, 0)

    {
      val r = Await.result(metadataConsumer
                             .ask(Metadata.GetEndOffsets(Set(topicPartition)))
                             .mapTo[Metadata.EndOffsets],
                           timeout.duration)
      println(s"Meta for $topicPartition: $r")
    }

    implicit val mat: ActorMaterializer = ActorMaterializer()

    val lock = new CountDownLatch(max)

    println(s"Reading options: ${Subscriptions.assignmentWithOffset(topicPartition -> from)}")
    val consumer = Consumer
      .plainSource(consumerSettings, Subscriptions.assignmentWithOffset(topicPartition, from))
      .take(max)
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

package com.wavesplatform

import java.time.{Duration, LocalDate, ZoneOffset}
import java.util
import java.util.Properties

import com.wavesplatform.events.protobuf.BlockchainUpdated
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.{Deserializer, IntegerDeserializer}

import scala.collection.JavaConverters._

package object data {
  object IntDeserializer extends Deserializer[Int] {
    private[this] val integerDeserializer                                      = new IntegerDeserializer
    override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = integerDeserializer.configure(configs, isKey)
    override def deserialize(topic: String, data: Array[Byte]): Int            = integerDeserializer.deserialize(topic, data)
    override def close(): Unit                                                 = ()
  }

  object BlockchainUpdatedDeserializer extends Deserializer[BlockchainUpdated] {
    override def configure(configs: util.Map[String, _], isKey: Boolean): Unit    = ()
    override def deserialize(topic: String, data: Array[Byte]): BlockchainUpdated = BlockchainUpdated.parseFrom(data)
    override def close(): Unit                                                    = ()
  }

  object Consumer {
    def create(): KafkaConsumer[Int, BlockchainUpdated] = {
      val props = new Properties
      props.put("bootstrap.servers", util.Arrays.asList(sys.env.getOrElse("VOLK_KAFKA", "kafka-dev.wvservices.com:9092")))
      props.put("group.id", "Volk s vol strit1ss")
      props.put("client.id", "Volk s vol strit")
      props.put("enable.auto.commit", "false")
      props.put("session.timeout.ms", "30000")
      props.put("max.poll.records", "1000")
      val consumer = new KafkaConsumer[Int, BlockchainUpdated](props, IntDeserializer, BlockchainUpdatedDeserializer)
      val topic    = sys.env.getOrElse("VOLK_TOPIC", "blockchain-updates-mainnet")
      if (sys.env.contains("VOLK_RESET")) {
        import scala.collection.JavaConverters._
        val partitions = consumer.partitionsFor(topic).asScala.map(pi => new TopicPartition(pi.topic(), pi.partition())).asJava
        consumer.assign(partitions)
        val ts = LocalDate.now().minusMonths(1).atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli
        val offsets = consumer.offsetsForTimes(partitions.asScala.map(_ -> java.lang.Long.valueOf(ts)).toMap.asJava)
        offsets.asScala.foreach { case (tp, offset) => consumer.seek(tp, offset.offset()) }
      } else consumer.subscribe(util.Arrays.asList(topic))
      consumer
    }
  }

  class PollingAgent {
    var isCommitPending = false
    val consumer        = Consumer.create()

    def start(cb: Seq[BlockchainUpdated] => Unit): Unit = {
      while (!Thread.currentThread().isInterrupted) {
        isCommitPending = false
        val records = this.consumer.poll(Duration.ofMillis(1000L))
        cb(records.asScala.map(_.value()).toVector)
        isCommitPending = true
        consumer.commitSync()
      }
    }

    def shutdown(): Unit = {
      if (this.isCommitPending) this.consumer.commitSync()
      this.consumer.close()
    }

    def shutdown(timeout: Duration): Unit = {
      if (this.isCommitPending) this.consumer.commitSync()
      this.consumer.close(Duration.ofMillis(timeout.toMillis))
    }
  }
}

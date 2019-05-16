package com.wavesplatform.events
import java.util

import com.wavesplatform.events.protobuf.PBEvents
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.state.{BlockAdded, BlockchainUpdated, MicroBlockAdded, MicroBlockRollbackCompleted, RollbackCompleted}
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerConfig, ProducerRecord}
import org.apache.kafka.common.serialization.{IntegerSerializer, Serializer}

package object kafka {
  private object BlockchainUpdatedSerializer extends Serializer[BlockchainUpdated] {
    override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
    override def close(): Unit                                                 = {}

    override def serialize(topic: String, data: BlockchainUpdated): Array[Byte] =
      PBEvents.protobuf(data).toByteArray
  }

  private object IntSerializer extends Serializer[Int] {
    val integerSerializer = new IntegerSerializer

    override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = integerSerializer.configure(configs, isKey)
    override def close(): Unit                                                 = integerSerializer.close()

    override def serialize(topic: String, data: Int): Array[Byte] =
      integerSerializer.serialize(topic, data)
  }

  private def createProperties(settings: BlockchainUpdatesSettings): util.Properties = {
    val props = new util.Properties()
    props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, settings.bootstrapServers)
    props.put(ProducerConfig.CLIENT_ID_CONFIG, settings.clientId)
    //  props.put(ProducerConfig.RETRIES_CONFIG, "0")
    props.put(ProducerConfig.ACKS_CONFIG, "all")
    props
  }

  def createProducer(settings: BlockchainUpdatesSettings): KafkaProducer[Int, BlockchainUpdated] =
    new KafkaProducer[Int, BlockchainUpdated](createProperties(settings), IntSerializer, BlockchainUpdatedSerializer)

  def createProducerRecord(topic: String, event: BlockchainUpdated): ProducerRecord[Int, BlockchainUpdated] = {
    val h = event match {
      case BlockAdded(_, height, _, _)      => height
      case MicroBlockAdded(_, height, _, _) => height
      case RollbackCompleted(_, height)     => height
      case MicroBlockRollbackCompleted(_, height)     => height
    }
    new ProducerRecord[Int, BlockchainUpdated](topic, h, event)
  }
}

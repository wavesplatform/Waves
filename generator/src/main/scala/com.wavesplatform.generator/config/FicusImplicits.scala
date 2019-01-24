package com.wavesplatform.generator.config

import com.google.common.base.CaseFormat
import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.{TransactionParser, TransactionParsers}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}

trait FicusImplicits {

  implicit val distributionsReader: ValueReader[Map[TransactionParser, Double]] = {
    val converter                                = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toTxType(key: String): TransactionParser = TransactionParsers.by(converter.convert(key)).get

    CollectionReaders.mapValueReader[Double].map { xs =>
      xs.map {
        case (k, v) => {
          toTxType(k) -> v
        }
      }
    }
  }

  implicit val dataEntryReader = new ValueReader[DataEntry[_]] {
    override def read(config: Config, path: String): DataEntry[_] = {
      val key = config.getConfig(path).getString("key")

      config.getConfig(path).getString("type") match {
        case "Integer" =>
          val value = config.getConfig(path).getLong("value")
          IntegerDataEntry(key, value)
        case "String" =>
          val value = config.getConfig(path).getString("value")
          StringDataEntry(key, value.asInstanceOf[String])
        case "Boolean" =>
          val value = config.getConfig(path).getBoolean("value")
          BooleanDataEntry(key, value.asInstanceOf[Boolean])
        case "Binary" =>
          val value = config.getConfig(path).getString("value")
          BinaryDataEntry(key, ByteStr.decodeBase58(value.asInstanceOf[String]).get)
      }
    }
  }
}

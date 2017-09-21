package com.wavesplatform.generator.config

import java.net.InetSocketAddress

import com.google.common.base.CaseFormat
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import scorex.transaction.TransactionParser.TransactionType

trait FicusImplicits {
  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    new InetSocketAddress(
      config.as[String](s"$path.address"),
      config.as[Int](s"$path.port")
    )
  }

  implicit val distributionsReader: ValueReader[Map[TransactionType.Value, Double]] = {
    val converter = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toTxType(key: String): TransactionType.Value = TransactionType.withName(s"${converter.convert(key)}Transaction")

    CollectionReaders.mapValueReader[Double].map { xs =>
      xs.map { case (k, v) => toTxType(k) -> v }
    }
  }
}

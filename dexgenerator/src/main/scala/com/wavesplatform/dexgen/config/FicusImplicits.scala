package com.wavesplatform.dexgen.config

import java.net.InetSocketAddress

import com.google.common.base.CaseFormat
import com.typesafe.config.Config

import com.wavesplatform.dexgen.utils.GenOrderType
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}

trait FicusImplicits {
  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    new InetSocketAddress(
      config.as[String](s"$path.address"),
      config.as[Int](s"$path.port")
    )
  }

  implicit val distributionsReader: ValueReader[Map[GenOrderType.Value, Double]] = {
    val converter                                    = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toOrderType(key: String): GenOrderType.Value = GenOrderType.withName(s"${converter.convert(key)}")

    CollectionReaders.mapValueReader[Double].map { xs =>
      xs.map { case (k, v) => toOrderType(k) -> v }
    }
  }
}

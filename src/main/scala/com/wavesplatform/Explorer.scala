package com.wavesplatform

import java.io.File

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.openDB
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.ByteStr
import org.slf4j.bridge.SLF4JBridgeHandler
import scorex.account.AddressScheme
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.util.Try

object Explorer extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename = Try(args(0)).toOption.getOrElse("waves-testnet.conf")
    val maybeOrderId   = Try(args(1)).toOption

    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val db     = openDB(settings.dataDirectory, settings.levelDbCacheSize)
    val reader = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")

    val orderId = maybeOrderId.flatMap(Base58.decode(_).toOption).map(ByteStr.apply)
    if (orderId.isDefined) {
      val kVolumeAndFee = LevelDBWriter.k.filledVolumeAndFee(blockchainHeight, orderId.get)
      val bytes1        = db.get(kVolumeAndFee.keyBytes)
      val v             = kVolumeAndFee.parse(bytes1)
      log.info(s"OrderId = ${Base58.encode(orderId.get.arr)}: Volume = ${v.volume}, Fee = ${v.fee}")

      val kVolumeAndFeeHistory = LevelDBWriter.k.filledVolumeAndFeeHistory(orderId.get)
      val bytes2               = db.get(kVolumeAndFeeHistory.keyBytes)
      val value2               = kVolumeAndFeeHistory.parse(bytes2)
      val value2Str            = value2.mkString("[", ", ", "]")
      log.info(s"OrderId = ${Base58.encode(orderId.get.arr)}: History = $value2Str")
      value2.foreach { h =>
        val k = LevelDBWriter.k.filledVolumeAndFee(h, orderId.get)
        val v = k.parse(db.get(k.keyBytes))
        log.info(s"\t h = $h: Volume = ${v.volume}, Fee = ${v.fee}")
      }
    } else log.error("No order ID was provided")

  }
}

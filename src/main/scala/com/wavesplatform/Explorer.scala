package com.wavesplatform

import java.io.File

import com.typesafe.config.ConfigFactory
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.openDB
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.ByteStr
import org.slf4j.bridge.SLF4JBridgeHandler
import scorex.account.{Address, AddressScheme}
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.util.Try

object Explorer extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename = Try(args(0)).toOption.getOrElse("waves-testnet.conf")

    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    log.info(s"Data directory: ${settings.dataDirectory}")

    val db     = openDB(settings.dataDirectory, settings.levelDbCacheSize)
    val reader = new LevelDBWriter(db, settings.blockchainSettings.functionalitySettings)

    val blockchainHeight = reader.height
    log.info(s"Blockchain height is $blockchainHeight")
    try {

      val flag      = args(1).toUpperCase
      val primaryId = args(2)

      flag match {
        case "O" =>
          val orderId = Base58.decode(primaryId).toOption.map(ByteStr.apply)
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

        case "A" =>
          val address   = Address.fromString(primaryId).right.get
          val aid       = LevelDBWriter.k.addressId(address)
          val addressId = aid.parse(db.get(aid.keyBytes)).get
          log.info(s"Address id = $addressId")

          val kwbh = LevelDBWriter.k.wavesBalanceHistory(addressId)
          val wbh  = kwbh.parse(db.get(kwbh.keyBytes))

          val balances = wbh.map { h =>
            val k = LevelDBWriter.k.wavesBalance(h, addressId)
            h -> k.parse(db.get(k.keyBytes))
          }
          balances.foreach(b => log.info(s"h = ${b._1}: balance = ${b._2}"))

        case "T" =>
          val address   = Address.fromString(primaryId).right.get
          val aid       = LevelDBWriter.k.addressId(address)
          val addressId = aid.parse(db.get(aid.keyBytes)).get
          log.info(s"Address id = $addressId")
          val ktxidh = LevelDBWriter.k.addressTransactionIds(args(3).toInt, addressId)

          for ((t, id) <- ktxidh.parse(db.get(ktxidh.keyBytes))) {
            log.info(s"$id of type $t")
          }

        case "AA" =>
          val secondaryId = args(3)

          val address   = Address.fromString(primaryId).right.get
          val asset     = ByteStr.decodeBase58(secondaryId).get
          val ai        = LevelDBWriter.k.addressId(address)
          val addressId = ai.parse(db.get(ai.keyBytes)).get
          log.info(s"Address ID = $addressId")

          val kabh = LevelDBWriter.k.assetBalanceHistory(addressId, asset)
          val abh  = kabh.parse(db.get(kabh.keyBytes))

          val balances = abh.map { h =>
            val k = LevelDBWriter.k.assetBalance(h, addressId, asset)
            h -> k.parse(db.get(k.keyBytes))
          }
          balances.foreach(b => log.info(s"h = ${b._1}: balance = ${b._2}"))
      }
    } finally db.close()
  }
}

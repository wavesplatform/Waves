package com.wavesplatform.events.settings

case class BlockchainUpdatesSettings(bootstrapServers: String, topic: String, clientId: String)

object BlockchainUpdatesSettings {
  import net.ceedubs.ficus.readers.{NameMapper, ValueReader}
  import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader

  implicit val chosenCase: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

  implicit val valueReader: ValueReader[BlockchainUpdatesSettings] = arbitraryTypeValueReader

//  val configPath = "blockchain-updates"
//  def fromConfig(c: Config): BlockchainUpdatesSettings = {
//    val config = c.getConfig(configPath)
//
//    BlockchainUpdatesSettings(
//      config.as[String]("bootstrap-servers"),
//      config.as[String]("topic"),
//      config.as[String]("client-id")
//    )
//  }
}

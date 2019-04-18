package com.wavesplatform.events.settings

case class BlockchainUpdatesSettings(bootstrapServers: String, topic: String, clientId: String)

object BlockchainUpdatesSettings {
  import net.ceedubs.ficus.Ficus._
  import net.ceedubs.ficus.readers.ValueReader
  import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
  import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

  implicit val valueReader: ValueReader[BlockchainUpdatesSettings] = arbitraryTypeValueReader
}

package com.wavesplatform.events.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

case class SslSettings(enabled: Boolean, username: String, password: String)

object SslSettings {
  implicit val valueReader: ValueReader[SslSettings] = arbitraryTypeValueReader
}

case class BlockchainUpdatesSettings(bootstrapServers: String, topic: String, clientId: String, ssl: SslSettings)

object BlockchainUpdatesSettings {
  implicit val valueReader: ValueReader[BlockchainUpdatesSettings] = arbitraryTypeValueReader
}

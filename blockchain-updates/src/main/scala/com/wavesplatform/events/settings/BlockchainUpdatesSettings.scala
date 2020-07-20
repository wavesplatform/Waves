package com.wavesplatform.events.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase

case class BlockchainUpdatesSettings(httpPort: Int, grpcPort: Int, directory: String)

object BlockchainUpdatesSettings {
  implicit val valueReader: ValueReader[BlockchainUpdatesSettings] = arbitraryTypeValueReader
}

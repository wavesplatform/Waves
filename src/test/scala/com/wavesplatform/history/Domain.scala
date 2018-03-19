package com.wavesplatform.history

import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.Address
import scorex.transaction.{BlockchainUpdater, History}

case class Domain(history: History, stateReader: SnapshotStateReader, blockchainUpdater: BlockchainUpdater) {
  def effBalance(a: Address): Long = stateReader.effectiveBalance(a, stateReader.height, 1000)
}

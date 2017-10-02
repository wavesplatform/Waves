package com.wavesplatform.history

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.state2.reader.StateReader
import scorex.account.Address
import scorex.transaction.{BlockchainUpdater, History}

case class Domain(history: History, stateReader: StateReader, blockchainUpdater: BlockchainUpdater) {
  def effBalance(a:Address): Long = stateReader.effectiveBalanceAtHeightWithConfirmations(a, stateReader.height, 1000).get
}

package com.wavesplatform.history

import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.{BlockchainUpdater, History}

case class Domain(history: History, stateReader: StateReader, blockchainUpdater: BlockchainUpdater)
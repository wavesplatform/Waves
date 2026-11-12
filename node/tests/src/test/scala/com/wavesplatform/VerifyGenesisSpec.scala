package com.wavesplatform

import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.Miner
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxValidationError.GenericError

class VerifyGenesisSpec extends FreeSpec with WithDomain {
  private def withConfiguredGenesis(f: Domain => Unit): Unit =
    withDomain() { d =>
      checkGenesis(d.settings, d.blockchainUpdater, Miner.StrictDisabledMiner)
      f(d)
    }

  private def withAnotherGenesis(settings: WavesSettings): WavesSettings = {
    val bs = settings.blockchainSettings
    val gs = bs.genesisSettings
    settings.copy(blockchainSettings = bs.copy(genesisSettings = gs.copy(blockTimestamp = gs.blockTimestamp + 1, signature = None)))
  }

  "verifyGenesis" - {
    "accepts a blockchain started from the configured genesis block" in withConfiguredGenesis { d =>
      verifyGenesis(d.settings, d.blockchain) shouldBe Right(())
    }

    "rejects a blockchain started from another genesis block" in withConfiguredGenesis { d =>
      verifyGenesis(withAnotherGenesis(d.settings), d.blockchain) shouldBe Left(GenericError("Mismatched genesis blocks in configuration and blockchain"))
    }

    "rejects an empty blockchain without writing to it" in withDomain() { d =>
      verifyGenesis(d.settings, d.blockchain) shouldBe Left(GenericError("Blockchain is empty"))
      d.blockchain.height shouldBe 0
    }
  }
}

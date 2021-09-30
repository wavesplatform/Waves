package com.wavesplatform.state.rollback

import com.wavesplatform.db.WithDomain
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.EthHelpers

class EthereumTransactionRollbackSpec extends FlatSpec with WithDomain with EthHelpers {
  "Ethereum transfer" should "rollback" in withDomain(DomainPresets.RideV6) { d =>
    val transaction = EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 1, Waves)

    withClue("genesis") {
      d.helpers.creditWavesToDefaultSigner(1 + 100000)
      d.balance(TxHelpers.defaultEthAddress) shouldBe (1 + 100000)
      d.balance(TxHelpers.secondAddress) shouldBe 0
    }

    withClue("after transaction") {
      d.appendBlock(transaction)
      d.balance(TxHelpers.defaultEthAddress) shouldBe 0
      d.balance(TxHelpers.secondAddress) shouldBe 1
    }

    withClue("after rollback") {
      d.rollbackTo(1)
      d.balance(TxHelpers.defaultEthAddress) shouldBe (1 + 100000)
      d.balance(TxHelpers.secondAddress) shouldBe 0
    }
  }
}

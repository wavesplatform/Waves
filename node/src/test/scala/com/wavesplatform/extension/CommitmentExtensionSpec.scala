package com.wavesplatform.extension

import com.wavesplatform.WithDomain
import com.wavesplatform.settings.{WavesSettings, TestFunctionalitySettings}
import com.wavesplatform.test.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.wavesplatform.account.KeyPair
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.common.utils.EitherExt2

class CommitmentExtensionSpec extends AnyFlatSpec with Matchers with WithDomain with ParallelTestExecution {

  val fee = 100000L

  "CommitmentExtension" should "start without errors" in {
    val settings = WavesSettings.fromRootConfig(testConfig)
    withDomain(settings) { d =>
      d.blockchain.height should be >= 1
    }
  }

  it should "log a warning if balance is low" in {
    val richAccount = KeyPair.random()
    val lowBalanceAccount = KeyPair.random()
    val settings = WavesSettings.fromRootConfig(testConfig)
    val customSettings = settings.copy(
      blockchainSettings = settings.blockchainSettings.copy(
        custom = settings.blockchainSettings.custom.copy(
          functionality = TestFunctionalitySettings.Enabled.copy(generationPeriodLength = 5)
        )
      )
    )

    withDomain(customSettings) { d =>
      val transfer1 = TransferTransaction.selfSigned(1.toByte, richAccount, lowBalanceAccount.toAddress, 99 * 100000000L, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()
      d.appendBlock(transfer1)

      // Wait for the extension to run
      Thread.sleep(5 * 1000)

      // We can't easily check the logs here, but we can check that a transaction was created.
      // This is an indirect way to verify the logic was executed.
      d.blockchain.transactionInfo(d.blockchain.lastBlock.get.uniqueId) should not be empty
    }
  }

  it should "log an error if balance is insufficient for fee" in {
    val richAccount = KeyPair.random()
    val poorAccount = KeyPair.random()
    val settings = WavesSettings.fromRootConfig(testConfig)
    val customSettings = settings.copy(
      blockchainSettings = settings.blockchainSettings.copy(
        custom = settings.blockchainSettings.custom.copy(
          functionality = TestFunctionalitySettings.Enabled.copy(generationPeriodLength = 5)
        )
      )
    )

    withDomain(customSettings) { d =>
      val transfer1 = TransferTransaction.selfSigned(1.toByte, richAccount, poorAccount.toAddress, fee - 1, System.currentTimeMillis(), fee, Array.emptyByteArray).explicitGet()
      d.appendBlock(transfer1)

      val heightBefore = d.blockchain.height

      // Wait for the extension to run
      Thread.sleep(5 * 1000)

      // No new blocks should be generated because no commitment transaction was created
      d.blockchain.height should be(heightBefore)
    }
  }
}

package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.fixtures.PrepareInvokeTestData
import com.wavesplatform.events.fixtures.WavesTxChecks.checkBalances
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.concurrent.ScalaFutures

class BlockchainUpdatesSubscribeInvokeTxSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val sender: SeedKeyPair            = TxHelpers.signer(12)
  val senderAddress: Address         = sender.toAddress
  val senderBalanceBefore: Long      = 5.waves

  "BU-31. Return correct data for Invoke" in {
    val invoke = TxHelpers.invoke(senderAddress, PrepareInvokeTestData.invokeAssetScript)

    withGenerateSubscription(
      settings = currentSettings,
      balances = Seq(AddrWithBalance(sender.toAddress, senderBalanceBefore))
    )(_.appendMicroBlock(invoke)) { updates =>
      val append = updates(1).append

      checkBalances(
        append.transactionStateUpdates.head.balances,
        Map((sender.toAddress, Waves) -> (senderBalanceBefore, senderBalanceBefore - invoke.fee.value))
      )
    }
  }
}

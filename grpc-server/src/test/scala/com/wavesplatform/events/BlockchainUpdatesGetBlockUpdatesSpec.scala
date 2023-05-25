package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.events.fixtures.WavesTxChecks.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.concurrent.ScalaFutures


class BlockchainUpdatesGetBlockUpdatesSpec extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val customFee: Long = 5234567L
  val customAssetIssueFee = 234567654L
  val sender: SeedKeyPair = TxHelpers.signer(12)
  val senderAddress: Address = sender.toAddress
  val senderBalanceBefore: Long = 20.waves
  var senderBalanceBeforeTx: Long = 0L
  var senderBalanceAfterTx: Long = 0L

  "BlockchainUpdates getBlockUpdate tests" - {
    "BU- . Return correct data for alias" in {
      val aliasTx = TxHelpers.createAlias("test", sender, fee = customFee)
      withGenerateGetBlockUpdate(
        settings = currentSettings,
        balances = Seq(AddrWithBalance(senderAddress, senderBalanceBefore))
      )(_.appendMicroBlock(aliasTx)) { updates =>
        val append = updates.append
        checkCreateAlias(append.transactionIds.head, append.transactionAt(0), aliasTx)
        checkBalances(
          append.transactionStateUpdates.head.balances,
          Map((senderAddress, Waves) -> (senderBalanceBefore, senderBalanceBefore - customFee))
        )
      }
    }
  }
}

package com.wavesplatform.events

import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.events.fixtures.WavesTxChecks.{checkBalances, checkCreateAlias}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import com.wavesplatform.lang.script.Script
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{CreateAliasTransaction, TxHelpers}
import org.scalatest.concurrent.ScalaFutures

abstract class BlockchainUpdatesTestBase extends FreeSpec with WithBUDomain with ScalaFutures {
  val currentSettings: WavesSettings = DomainPresets.RideV6
  val customFee: Long                = 5234567L
  val customAssetIssueFee            = 234567654L
  val sender: SeedKeyPair            = TxHelpers.signer(12)
  val senderAddress: Address         = sender.toAddress
  val senderBalanceBefore: Long      = 20.waves
  val testScript: Script = TxHelpers.script(s"""{-# STDLIB_VERSION 6 #-}
                                               |{-# CONTENT_TYPE DAPP #-}
                                               |{-# SCRIPT_TYPE ACCOUNT #-}
                                               |
                                               |@Verifier(tx)
                                               |func verify () = match(tx) {
                                               |    case _ =>
                                               |      if (
                                               |        ${(1 to 9).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || \n")}
                                               |      ) then true else true
                                               |}""".stripMargin)

  protected def checkAlias(append: Append, aliasTx: CreateAliasTransaction): Unit = {
    val senderBalanceAfterTx: Long = senderBalanceBefore - aliasTx.fee.value

    checkCreateAlias(append.transactionIds.head, append.transactionAt(0), aliasTx)
    checkBalances(
      append.transactionStateUpdates.head.balances,
      Map((senderAddress, Waves) -> (senderBalanceBefore, senderBalanceAfterTx))
    )
  }

}

package com.wavesplatform.http

import com.wavesplatform.TestValues
import com.wavesplatform.api.common.CommonGeneratorsApi
import com.wavesplatform.api.http.{GeneratorsApiRoute, RouteTimeout}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.diffs
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.utils.SharedSchedulerMixin
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.*

import scala.concurrent.duration.*

class GeneratorsApiRouteSpec extends RouteSpec("/generators") with RestAPISettingsHelper with SharedDomain with SharedSchedulerMixin {
  override def settings: WavesSettings = {
    val orig = DomainPresets.DeterministicFinality
    orig.copy(
      restAPISettings = restAPISettings,
      blockchainSettings = orig.blockchainSettings.copy(
        functionalitySettings = orig.blockchainSettings.functionalitySettings.copy(generationPeriodLength = 3)
      )
    )
  }

  private val wallet        = Wallet(WalletSettings(file = None, password = None, Some(ByteStr("seed".getBytes()))))
  private val generator     = wallet.generateNewAccounts(1).head
  private val depositAndFee = CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee
  private val balance       = diffs.ENOUGH_AMT + depositAndFee

  override def genesisBalances: Seq[WithState.AddrWithBalance] =
    AddrWithBalance(generator.toAddress, balance) +: AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)

  private val api = CommonGeneratorsApi(domain.rdb, domain.blockchainUpdater)
  private val route = seal(
    GeneratorsApiRoute(
      restAPISettings,
      api,
      domain.testTime,
      new RouteTimeout(60.seconds)(using sharedScheduler)
    ).route
  )

  routePath("/at/{height}") in {
    val generationPeriod = domain.blockchain.currentGenerationPeriod.value.next

    val txn    = TxHelpers.commitToGeneration(generationPeriod.start, sender = generator)
    val block1 = domain.createBlock(Block.PlainBlockVersion, Seq(txn), strictTime = true) // defaultSigner

    domain.appender.appendBlock(block1)
    domain.appendBlock()
    Get(routePath(s"/at/${domain.blockchain.height}")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr()
    }

    val block2 = domain.createBlock(Block.PlainBlockVersion, txs = Nil, strictTime = true, generator = generator)
    domain.appender.appendBlock(block2)
    Get(routePath(s"/at/${domain.blockchain.height}")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(
        Json.obj(
          "address"       -> generator.toAddress.toString,
          "balance"       -> (balance - depositAndFee),
          "transactionId" -> txn.id().toString
        )
      )
    }
  }
}

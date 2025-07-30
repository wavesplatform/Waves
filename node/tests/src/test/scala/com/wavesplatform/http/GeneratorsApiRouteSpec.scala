package com.wavesplatform.http

import com.wavesplatform.api.common.CommonGeneratorsApi
import com.wavesplatform.api.http.{GeneratorsApiRoute, RouteTimeout}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
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
        functionalitySettings = orig.blockchainSettings.functionalitySettings.copy(commitmentPeriod = 3)
      )
    )
  }

  private val wallet  = Wallet(WalletSettings(file = None, password = None, Some(ByteStr("seed".getBytes()))))
  private val miner   = wallet.generateNewAccounts(1).head
  private val balance = 1_000_000_000_000L

  override def genesisBalances: Seq[WithState.AddrWithBalance] = Seq(AddrWithBalance(miner.toAddress, balance))

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
    val generationPeriod = domain.blockchain.currentGenerationPeriod.next
    println(generationPeriod)
    val txn = TxHelpers.commitToGeneration(generationPeriod.start, sender = miner)
    domain.appendBlock(txn)
    println(domain.blockchain.containsTransaction(txn))
    domain.appendBlock()

    val height = Height(domain.blockchain.height)
    println(domain.generatorsApi.generators(height))
    Get(routePath(s"/at/$height")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(
        Json.obj(
          "address"       -> miner.toAddress.toString,
          "balance"       -> balance,
          "transactionId" -> txn.id().toString
        )
      )
    }
  }
}

package com.wavesplatform.http

import com.wavesplatform.TestValues
import com.wavesplatform.api.common.CommonGeneratorsApi
import com.wavesplatform.api.http.{GeneratorsApiRoute, RouteTimeout}
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsSignature}
import com.wavesplatform.db.WithState
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.{GeneratorIndex, Height, diffs}
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

  private val wallet                      = Wallet(WalletSettings(file = None, password = None, Some(ByteStr("seed".getBytes()))))
  private val Seq(generator1, generator2) = wallet.generateNewAccounts(2)
  private val depositAndFee               = CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee
  private val initBalance                 = diffs.ENOUGH_AMT + depositAndFee

  override def genesisBalances: Seq[WithState.AddrWithBalance] =
    Seq(generator1, generator2).map(x => AddrWithBalance(x.toAddress, initBalance)) ++ AddrWithBalance.enoughBalances(TxHelpers.defaultSigner)

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

    val txns   = Seq(generator1, generator2).map(x => TxHelpers.commitToGeneration(generationPeriod.start, sender = x))
    val block2 = domain.createBlock(Block.PlainBlockVersion, txns, strictTime = true) // defaultSigner

    domain.appender.appendBlock(block2)
    domain.appendBlock()
    Get(routePath(s"/at/${domain.blockchain.height}")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr()
    }

    val otherFinalizedBlockId = TxHelpers.randomBlockId
    val block4 = domain.createBlock(
      Block.PlainBlockVersion,
      txs = Nil,
      strictTime = true,
      generator = generator1,
      finalizationVoting = Some(
        FinalizationVoting(
          valid = Nil,
          finalizedHeight = Height(1),
          aggregatedEndorsement = BlsSignature.Empty,
          conflict = Vector(
            BlockEndorsement.signed(
              BlsKeyPair(generator2.privateKey),
              GeneratorIndex(1),
              otherFinalizedBlockId,
              finalizedHeight = Height(1),
              endorsedId = domain.blockchain.lastBlockId.value
            )
          )
        )
      )
    )
    domain.appender.appendBlock(block4)
    domain.appender.appendBlock(domain.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true))

    Get(routePath(s"/at/${domain.blockchain.height}")) ~> route ~> check {
      responseAs[JsValue] shouldBe Json.arr(
        Json.obj(
          "address"       -> generator1.toAddress.toString,
          "balance"       -> (initBalance - depositAndFee),
          "transactionId" -> txns.head.id().toString
        ),
        Json.obj(
          "address"        -> generator2.toAddress.toString,
          "balance"        -> (initBalance - depositAndFee),
          "transactionId"  -> txns.last.id().toString,
          "conflictHeight" -> 4
        )
      )
    }
  }
}

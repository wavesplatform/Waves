package com.wavesplatform

import scala.concurrent.Future

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.script.Script
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, Blockchain, Height, LeaseBalance, NG, VolumeAndFee}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.transaction.{Asset, TxHelpers}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import org.scalamock.MockFactoryBase
import org.scalamock.matchers.MockParameter

trait BlockchainStubHelpers { self: MockFactoryBase =>
  def createBlockchainStub(f: Blockchain => Unit = _ => ()): Blockchain with NG = {
    trait Blockchain1 extends Blockchain with NG
    val blockchain = stub[Blockchain1]
    f(blockchain) // Overrides
    (() => blockchain.settings).when().returns(WavesSettings.default().blockchainSettings)
    (() => blockchain.activatedFeatures)
      .when()
      .returns(
        Map(
          BlockchainFeatures.BlockV5.id             -> 0,
          BlockchainFeatures.SmartAccounts.id       -> 0,
          BlockchainFeatures.SmartAssets.id         -> 0,
          BlockchainFeatures.SmartAccountTrading.id -> 0,
          BlockchainFeatures.OrderV3.id             -> 0,
          BlockchainFeatures.Ride4DApps.id          -> 0
        )
      )
    (blockchain.accountScript _).when(*).returns(None)
    (blockchain.leaseBalance _).when(*).returns(LeaseBalance.empty)
    (() => blockchain.height).when().returns(1)
    (blockchain.blockHeader _).when(*).returns {
      val block = TestBlock.create(Nil)
      Some(SignedBlockHeader(block.header, block.signature))
    }
    (blockchain.filledVolumeAndFee _).when(*).returns(VolumeAndFee.empty)
    (blockchain.assetDescription _).when(*).returns(None)
    (blockchain.balance _).when(TxHelpers.defaultAddress, Waves).returns(Long.MaxValue / 3)
    blockchain
  }

  def createTxPublisherStub(blockchain: Blockchain): TransactionPublisher = { (transaction, _) =>
    Future.successful(TransactionDiffer(blockchain.lastBlockTimestamp, System.currentTimeMillis())(blockchain, transaction).map(_ => true))
  }

  case class StubHelpers(blockchain: Blockchain) {
    def creditBalance(address: MockParameter[Address], asset: MockParameter[Asset], amount: Long = Long.MaxValue / 3): Unit = {
      (blockchain.balance _).when(address, asset).returns(amount)
    }

    def issueAsset(id: ByteStr, script: Option[Script] = None): Unit = {
      (blockchain.assetDescription _)
        .when(IssuedAsset(id))
        .returns(
          Some(
            AssetDescription(
              id,
              TestValues.keyPair.publicKey,
              ByteString.copyFromUtf8("test"),
              ByteString.copyFromUtf8("test"),
              8,
              reissuable = false,
              10000,
              Height(1),
              script.map(script => AssetScriptInfo(script, 1L)),
              0L,
              nft = false
            )
          )
        )
      (blockchain.assetScript _).when(IssuedAsset(id)).returns(script.map(script => AssetScriptInfo(script, 1L)))
    }

    def setScript(address: Address, script: Script): Unit = {
      (blockchain.accountScript _)
        .when(address)
        .returns(Some(AccountScriptInfo(PublicKey(new Array[Byte](32)), script, 1L, Map.empty.withDefaultValue(Map.empty.withDefaultValue(1L)))))
    }
  }
}

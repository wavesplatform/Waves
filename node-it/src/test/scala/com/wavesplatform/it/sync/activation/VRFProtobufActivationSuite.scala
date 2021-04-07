package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.{CustomValidationError, StateCheckFailed}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs, RandomKeyPair}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.concurrent.duration._

class VRFProtobufActivationSuite extends BaseFunSuite {
  val activationHeight = 9
  val updateInterval   = 3
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5, activationHeight)))
      .overrideBase(_.raw(s"waves.blockchain.custom.functionality.min-asset-info-update-interval = $updateInterval"))
      .buildNonConflicting()

  private lazy val senderAcc             = RandomKeyPair()
  private lazy val recipientAcc     = RandomKeyPair()
  private var assetId               = ""
  private var otherAssetId          = ""
  private var secondUpdateAssetTxId = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    miner.transfer(miner.keyPair, senderAcc.toAddress.toString, 100.waves, minFee, waitForTx = true)
    val (defaultName, defaultDescription) = ("asset", "description")
    assetId = miner.broadcastIssue(senderAcc, defaultName, defaultDescription, someAssetAmount, 8, true, script = None, waitForTx = true).id
    miner.waitForHeight(7, 3.minutes)
    otherAssetId = miner.broadcastIssue(senderAcc, defaultName, defaultDescription, someAssetAmount, 8, true, script = None, waitForTx = true).id
  }

  test("miner generates block v4 before activation") {
    val blockBeforeActivationHeight        = miner.blockAt(miner.height)
    val blockHeadersBeforeActivationHeight = miner.blockHeadersAt(miner.height)
    blockBeforeActivationHeight.version.get shouldBe Block.RewardBlockVersion
    blockHeadersBeforeActivationHeight.version.get shouldBe Block.RewardBlockVersion
    Base58.decode(blockBeforeActivationHeight.generationSignature.get).length shouldBe Block.GenerationSignatureLength
    blockBeforeActivationHeight.baseTarget shouldBe blockHeadersBeforeActivationHeight.baseTarget
  }

  test("not able to broadcast tx of new versions before activation") {
    assertApiError(miner.transfer(senderAcc, recipientAcc.toAddress.toString, transferAmount, version = TxVersion.V3)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(Ride V4, VRF, Protobuf, Failed transactions feature has not been activated yet)"
      error.id shouldBe 112
    }
  }

  test("not able to broadcast UpdateAssetInfoTransaction before activation") {
    assertApiError(miner.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(Ride V4, VRF, Protobuf, Failed transactions feature has not been activated yet)"
      error.id shouldBe 112
    }
  }

  test("only able to get block by signature (that is equal to id) before activation") {
    miner.blockById(miner.blockAt(miner.height).signature) shouldBe miner.blockAt(miner.height)
    miner.blockAt(miner.height).signature shouldBe miner.blockAt(miner.height).id
    Base58.decode(miner.blockAt(miner.height).signature).length shouldBe crypto.SignatureLength
    Base58.decode(miner.blockAt(miner.height).id).length shouldBe crypto.SignatureLength
  }

  test("not able to broadcast ExchangeTransaction with reversed buy/sell orders") {
    val (buyOrder, sellOrder) = mkOrders

    assertApiError(
      miner.broadcastExchange(
        miner.keyPair,
        sellOrder,
        buyOrder,
        buyOrder.amount,
        buyOrder.price,
        matcherFee,
        matcherFee,
        matcherFee,
        validate = false
      )
    ) { error =>
      error.id shouldBe CustomValidationError.Id
      error.message shouldBe "order1 should have OrderType.BUY"
    }
  }

  test("not able to update asset info after activation if update interval has not been reached after asset issue") {
    miner.waitForHeight(activationHeight, 2.minutes)
    assertApiError(miner.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(s"Can't update info of asset with id=$otherAssetId")
    }
  }

  test("miner generates block v5 after activation") {
    val blockAtActivationHeight        = miner.blockAt(miner.height)
    val blockHeadersAtActivationHeight = miner.blockHeadersAt(miner.height)
    blockAtActivationHeight.version.get shouldBe Block.ProtoBlockVersion
    blockHeadersAtActivationHeight.version.get shouldBe Block.ProtoBlockVersion

    val blockHeaderById = miner.blockHeaderForId(blockHeadersAtActivationHeight.id)
    blockHeaderById shouldBe blockHeadersAtActivationHeight
  }

  test("only able to get block by id (that is not equal to signature) after activation") {
    miner.blockById(miner.blockAt(miner.height).id) shouldBe miner.blockAt(miner.height)
    miner.blockAt(miner.height).signature should not be miner.blockAt(miner.height).id
    Base58.decode(miner.blockAt(miner.height).signature).length shouldBe crypto.SignatureLength
    Base58.decode(miner.blockAt(miner.height).id).length shouldBe crypto.DigestLength
  }

  test("able to broadcast UpdateAssetInfoTransaction if interval's reached before activation") {
    miner.updateAssetInfo(senderAcc, assetId, "updatedName", "updatedDescription", minFee, waitForTx = true)
  }

  test("able to broadcast UpdateAssetInfoTransaction after activation") {
    val nextTerm = miner.transactionInfo[TransactionInfo](otherAssetId).height + updateInterval + 1
    miner.waitForHeight(nextTerm, 2.minutes)
    secondUpdateAssetTxId = miner.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee, waitForTx = true)._1.id
  }

  test("able to broadcast tx of new version after activation") {
    val senderWavesBalance    = miner.balanceDetails(senderAcc.toAddress.toString)
    val recipientWavesBalance = miner.balanceDetails(recipientAcc.toAddress.toString)
    miner.transfer(senderAcc, recipientAcc.toAddress.toString, transferAmount, version = TxVersion.V3, waitForTx = true)

    miner.balanceDetails(senderAcc.toAddress.toString).available shouldBe senderWavesBalance.available - transferAmount - minFee
    miner.balanceDetails(recipientAcc.toAddress.toString).available shouldBe recipientWavesBalance.available + transferAmount
  }

  test("able to broadcast ExchangeTransaction with reversed buy/sell orders") {
    val (buyOrder, sellOrder) = mkOrders

    assertApiError(
      miner.broadcastExchange(
        miner.keyPair,
        sellOrder,
        buyOrder,
        buyOrder.amount,
        buyOrder.price,
        matcherFee,
        matcherFee * 10,
        matcherFee * 10,
        validate = false
      )
    ) { error =>
      error.id shouldBe CustomValidationError.Id
      error.message shouldBe "order1 should have OrderType.BUY"
    }

    miner.broadcastExchange(
      miner.keyPair,
      sellOrder,
      buyOrder,
      buyOrder.amount,
      buyOrder.price,
      matcherFee,
      matcherFee,
      matcherFee,
      version = TxVersion.V3,
      waitForTx = true
    )
  }

  test("rollback to height before activation/at activation/after activation height") {
    //rollback to activation height
    nodes.blacklistPeersAndRollback(activationHeight, returnToUTX = true)

    val blockAtActivationHeight1 = miner.blockAt(activationHeight)
    blockAtActivationHeight1.version.get shouldBe Block.ProtoBlockVersion
    val returnedTxIds = miner.utx().map(tx => tx.id).filter(_ != secondUpdateAssetTxId)

    miner.waitForHeight(activationHeight + 1, 2.minutes)
    val blockAfterActivationHeight1 = miner.blockAt(activationHeight + 1)
    blockAfterActivationHeight1.version.get shouldBe Block.ProtoBlockVersion
    nodes.waitForHeightArise()

    returnedTxIds.foreach(miner.waitForTransaction(_, timeout = 8 minutes))

    //rollback to height one block before activation height
    nodes.blacklistPeersAndRollback(activationHeight - 1, returnToUTX = false)

    val blockBeforeActivationHeight = miner.blockAt(activationHeight - 1)
    blockBeforeActivationHeight.version.get shouldBe Block.RewardBlockVersion

    miner.waitForHeight(activationHeight, 2.minutes)
    val blockAtActivationHeight2 = miner.blockAt(activationHeight)
    blockAtActivationHeight2.version.get shouldBe Block.ProtoBlockVersion

    miner.waitForHeight(activationHeight + 1, 2.minutes)
    val blockAfterActivationHeight2 = miner.blockAt(activationHeight + 1)
    blockAfterActivationHeight2.version.get shouldBe Block.ProtoBlockVersion
    nodes.waitForHeightArise()

    //rollback to height after activation height using rollback to block with signature method
    nodes.rollbackToBlockId(miner.blockAt(activationHeight + 1).id)

    val blockAtActivationHeight3 = miner.blockAt(activationHeight + 1)
    blockAtActivationHeight3.version.get shouldBe Block.ProtoBlockVersion
  }

  private def mkOrders: (Order, Order) = {
    val ts     = System.currentTimeMillis()
    val amount = 1000000
    val price  = 1000
    val buyOrder = Order.buy(
      version = TxVersion.V2,
      miner.keyPair,
      miner.publicKey,
      AssetPair.createAssetPair("WAVES", assetId).get,
      amount,
      price,
      ts,
      ts + Order.MaxLiveTime,
      matcherFee
    )
    val sellOrder = Order.sell(
      version = TxVersion.V2,
      miner.keyPair,
      miner.publicKey,
      AssetPair.createAssetPair("WAVES", assetId).get,
      amount,
      price,
      ts,
      ts + Order.MaxLiveTime,
      matcherFee
    )
    (buyOrder, sellOrder)
  }
}

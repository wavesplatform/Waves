package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.{CustomValidationError, StateCheckFailed}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.{TxExchangePrice, TxVersion}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.concurrent.duration.*

class VRFProtobufActivationSuite extends BaseTransactionSuite {
  val activationHeight = 9
  val updateInterval   = 3
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5.id, activationHeight)))
      .overrideBase(_.raw(s"waves.blockchain.custom.functionality.min-asset-info-update-interval = $updateInterval"))
      .buildNonConflicting()

  private def senderAcc             = firstKeyPair
  private def recipientAcc          = secondKeyPair
  private var assetId               = ""
  private var otherAssetId          = ""
  private var secondUpdateAssetTxId = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val (defaultName, defaultDescription) = ("asset", "description")
    assetId =
      sender.broadcastIssue(senderAcc, defaultName, defaultDescription, someAssetAmount, 8, reissuable = true, script = None, waitForTx = true).id
    sender.waitForHeight(7, 3.minutes)
    otherAssetId =
      sender.broadcastIssue(senderAcc, defaultName, defaultDescription, someAssetAmount, 8, reissuable = true, script = None, waitForTx = true).id
  }

  test("miner generates block v4 before activation") {
    val blockBeforeActivationHeight        = sender.blockAt(sender.height)
    val blockHeadersBeforeActivationHeight = sender.blockHeadersAt(sender.height)
    blockBeforeActivationHeight.version.get shouldBe Block.RewardBlockVersion
    blockHeadersBeforeActivationHeight.version.get shouldBe Block.RewardBlockVersion
    Base58.decode(blockBeforeActivationHeight.generationSignature.get).length shouldBe Block.GenerationSignatureLength
    blockBeforeActivationHeight.baseTarget shouldBe blockHeadersBeforeActivationHeight.baseTarget
  }

  test("not able to broadcast tx of new versions before activation") {
    assertApiError(sender.transfer(senderAcc, recipientAcc.toAddress.toString, transferAmount, version = TxVersion.V3)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(Ride V4, VRF, Protobuf, Failed transactions feature has not been activated yet)"
      error.id shouldBe 112
    }
  }

  test("not able to broadcast UpdateAssetInfoTransaction before activation") {
    assertApiError(sender.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(Ride V4, VRF, Protobuf, Failed transactions feature has not been activated yet)"
      error.id shouldBe 112
    }
  }

  test("only able to get block by signature (that is equal to id) before activation") {
    sender.blockById(sender.blockAt(sender.height).signature) shouldBe sender.blockAt(sender.height)
    sender.blockAt(sender.height).signature shouldBe sender.blockAt(sender.height).id
    Base58.decode(sender.blockAt(sender.height).signature).length shouldBe crypto.SignatureLength
    Base58.decode(sender.blockAt(sender.height).id).length shouldBe crypto.SignatureLength
  }

  test("not able to broadcast ExchangeTransaction with reversed buy/sell orders") {
    val (buyOrder, sellOrder) = mkOrders

    assertApiError(
      sender.broadcastExchange(
        sender.keyPair,
        sellOrder,
        buyOrder,
        buyOrder.amount,
        TxExchangePrice.unsafeFrom(buyOrder.price.value),
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
    sender.waitForHeight(activationHeight, 2.minutes)
    assertApiError(sender.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(s"Can't update info of asset with id=$otherAssetId")
    }
  }

  test("miner generates block v5 after activation") {
    val blockAtActivationHeight        = sender.blockAt(sender.height)
    val blockHeadersAtActivationHeight = sender.blockHeadersAt(sender.height)
    blockAtActivationHeight.version.get shouldBe Block.ProtoBlockVersion
    blockHeadersAtActivationHeight.version.get shouldBe Block.ProtoBlockVersion

    val blockHeaderById = sender.blockHeaderForId(blockHeadersAtActivationHeight.id)
    blockHeaderById shouldBe blockHeadersAtActivationHeight
  }

  test("only able to get block by id (that is not equal to signature) after activation") {
    sender.blockById(sender.blockAt(sender.height).id) shouldBe sender.blockAt(sender.height)
    sender.blockAt(sender.height).signature should not be sender.blockAt(sender.height).id
    Base58.decode(sender.blockAt(sender.height).signature).length shouldBe crypto.SignatureLength
    Base58.decode(sender.blockAt(sender.height).id).length shouldBe crypto.DigestLength
  }

  test("able to broadcast UpdateAssetInfoTransaction if interval's reached before activation") {
    sender.updateAssetInfo(senderAcc, assetId, "updatedName", "updatedDescription", minFee, waitForTx = true)
  }

  test("able to broadcast UpdateAssetInfoTransaction after activation") {
    val nextTerm = sender.transactionInfo[TransactionInfo](otherAssetId).height + updateInterval + 1
    sender.waitForHeight(nextTerm, 2.minutes)
    secondUpdateAssetTxId = sender.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee, waitForTx = true)._1.id
  }

  test("able to broadcast tx of new version after activation") {
    val senderWavesBalance    = sender.balanceDetails(senderAcc.toAddress.toString)
    val recipientWavesBalance = sender.balanceDetails(recipientAcc.toAddress.toString)
    sender.transfer(senderAcc, recipientAcc.toAddress.toString, transferAmount, version = TxVersion.V3, waitForTx = true)

    sender.balanceDetails(senderAcc.toAddress.toString).available shouldBe senderWavesBalance.available - transferAmount - minFee
    sender.balanceDetails(recipientAcc.toAddress.toString).available shouldBe recipientWavesBalance.available + transferAmount
  }

  test("able to broadcast ExchangeTransaction with reversed buy/sell orders") {
    val (buyOrder, sellOrder) = mkOrders

    assertApiError(
      sender.broadcastExchange(
        sender.keyPair,
        sellOrder,
        buyOrder,
        buyOrder.amount,
        TxExchangePrice.unsafeFrom(buyOrder.price.value),
        matcherFee,
        matcherFee * 10,
        matcherFee * 10,
        validate = false
      )
    ) { error =>
      error.id shouldBe CustomValidationError.Id
      error.message shouldBe "order1 should have OrderType.BUY"
    }

    sender.broadcastExchange(
      sender.keyPair,
      sellOrder,
      buyOrder,
      buyOrder.amount,
      TxExchangePrice.unsafeFrom(buyOrder.price.value),
      matcherFee,
      matcherFee,
      matcherFee,
      version = TxVersion.V3,
      waitForTx = true
    )
  }

  test("rollback to height before activation/at activation/after activation height") {
    // rollback to activation height
    nodes.rollback(activationHeight)

    val blockAtActivationHeight1 = sender.blockAt(activationHeight)
    blockAtActivationHeight1.version.get shouldBe Block.ProtoBlockVersion
    val returnedTxIds = sender.utx().map(tx => tx.id).filter(_ != secondUpdateAssetTxId)

    sender.waitForHeight(activationHeight + 1, 2.minutes)
    val blockAfterActivationHeight1 = sender.blockAt(activationHeight + 1)
    blockAfterActivationHeight1.version.get shouldBe Block.ProtoBlockVersion
    nodes.waitForHeightArise()

    returnedTxIds.foreach(sender.waitForTransaction(_, timeout = 8 minutes))

    // rollback to height one block before activation height
    nodes.rollback(activationHeight - 1, returnToUTX = false)

    val blockBeforeActivationHeight = sender.blockAt(activationHeight - 1)
    blockBeforeActivationHeight.version.get shouldBe Block.RewardBlockVersion

    sender.waitForHeight(activationHeight, 2.minutes)
    val blockAtActivationHeight2 = sender.blockAt(activationHeight)
    blockAtActivationHeight2.version.get shouldBe Block.ProtoBlockVersion

    sender.waitForHeight(activationHeight + 1, 2.minutes)
    val blockAfterActivationHeight2 = sender.blockAt(activationHeight + 1)
    blockAfterActivationHeight2.version.get shouldBe Block.ProtoBlockVersion
    nodes.waitForHeightArise()

    nodes.rollback(activationHeight + 1)

    val blockAtActivationHeight3 = sender.blockAt(activationHeight + 1)
    blockAtActivationHeight3.version.get shouldBe Block.ProtoBlockVersion
  }

  private def mkOrders: (Order, Order) = {
    val ts     = System.currentTimeMillis()
    val amount = 1000000
    val price  = 1000
    val buyOrder = Order
      .buy(
        version = TxVersion.V2,
        sender.keyPair,
        sender.publicKey,
        AssetPair.createAssetPair("WAVES", assetId).get,
        amount,
        price,
        ts,
        ts + Order.MaxLiveTime / 2,
        matcherFee
      )
      .explicitGet()
    val sellOrder = Order
      .sell(
        version = TxVersion.V2,
        sender.keyPair,
        sender.publicKey,
        AssetPair.createAssetPair("WAVES", assetId).get,
        amount,
        price,
        ts,
        ts + Order.MaxLiveTime / 2,
        matcherFee
      )
      .explicitGet()
    (buyOrder, sellOrder)
  }
}

package com.wavesplatform.http

import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.{RouteTimeout, TransactionsApiRoute}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.test.SharedDomain
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.utils.SharedSchedulerMixin
import org.apache.pekko.http.scaladsl.model.{HttpResponse, StatusCodes}
import org.apache.pekko.http.scaladsl.server.Route
import org.scalatest.OptionValues
import play.api.libs.json.*

import scala.concurrent.duration.*

class ProtoVersionTransactionsSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with SharedDomain
    with OptionValues
    with SharedSchedulerMixin {

  private val MinFee: Long            = (0.001 * Constants.UnitsInWave).toLong
  private val DataTxFee: Long         = 15000000
  private val InvokeScriptTxFee: Long = 15000000
  private val MassTransferTxFee: Long = 15000000
  private val SetScriptFee: Long      = (0.01 * Constants.UnitsInWave).toLong
  private val SetAssetScriptFee: Long = Constants.UnitsInWave

  private val now: Long        = ntpNow
  private val account: KeyPair = domain.wallet.generateNewAccount().get
  private val asset            = TxHelpers.issue().asset
  private val attachment       = Array.fill(TransferTransaction.MaxAttachmentSize)(1: Byte)
  private val script           = TxHelpers.script("true")

  private val route: Route =
    Route.seal(TransactionsApiRoute(
      restAPISettings,
      domain.transactionsApi,
      domain.wallet,
      domain.blockchain,
      () => domain.blockchain,
      () => domain.utxPool.size,
      DummyTransactionPublisher.accepting,
      domain.testTime,
      new RouteTimeout(60.seconds)(using sharedScheduler)
    ).route)

  "Proto transactions should be able to broadcast " - {
    "CreateAliasTransaction" in {
      val aliasTxUnsigned = CreateAliasTransaction.create(TxVersion.V3, account.publicKey, "test", MinFee, now, Proofs.empty).explicitGet()

      val (proofs, aliasTxJson) = Post(routePath("/sign"), aliasTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, aliasTxUnsigned)
      }

      val aliasTx  = aliasTxUnsigned.copy(proofs = proofs)
      val base64Tx = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(aliasTx)))

      Post(routePath("/broadcast"), aliasTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe aliasTxJson
      }

      (aliasTx.json() \ "chainId").asOpt[Byte].value shouldBe aliasTx.chainId
      decode(base64Tx) shouldBe aliasTx
    }

    "IssueTransaction/ReissueTransaction/BurnTransaction" in {
      val quantity   = 1000
      val decimals   = 2.toByte
      val reissuable = true

      val issueTxUnsigned = IssueTransaction
        .create(
          TxVersion.V3,
          account.publicKey,
          "Test asset",
          "Test description",
          quantity,
          decimals,
          reissuable,
          script = None,
          MinIssueFee,
          now,
          Proofs.empty
        )
        .explicitGet()

      val (issueProofs, issueTxJson) = Post(routePath("/sign"), issueTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, issueTxUnsigned)
      }

      val issueTx        = issueTxUnsigned.copy(proofs = issueProofs)
      val base64IssueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(issueTx)))

      Post(routePath("/broadcast"), issueTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe issueTxJson
      }

      val reissueTxUnsigned = ReissueTransaction
        .create(TxVersion.V3, account.publicKey, issueTx.asset, quantity, reissuable, MinIssueFee, now, Proofs.empty)
        .explicitGet()

      val (reissueProofs, reissueTxJson) = Post(routePath("/sign"), reissueTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, reissueTxUnsigned)
      }

      val reissueTx        = reissueTxUnsigned.copy(proofs = reissueProofs)
      val base64reissueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(reissueTx)))

      Post(routePath("/broadcast"), reissueTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe reissueTxJson
      }

      val burnTxUnsigned =
        BurnTransaction.create(TxVersion.V3, account.publicKey, issueTx.asset, quantity, MinIssueFee, now, Proofs.empty).explicitGet()

      val (burnProofs, burnTxJson) = Post(routePath("/sign"), burnTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, burnTxUnsigned)
      }

      val burnTx        = burnTxUnsigned.copy(proofs = burnProofs)
      val base64BurnStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(burnTx)))

      Post(routePath("/broadcast"), burnTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe burnTxJson
      }

      decode(base64IssueStr) shouldBe issueTx
      decode(base64reissueStr) shouldBe reissueTx
      decode(base64BurnStr) shouldBe burnTx

      (issueTx.json() \ "chainId").asOpt[Byte].value shouldBe issueTx.chainId
      (reissueTx.json() \ "chainId").asOpt[Byte].value shouldBe reissueTx.chainId
      (burnTx.json() \ "chainId").asOpt[Byte].value shouldBe burnTx.chainId

      PBSince.affects(issueTx) shouldBe true
      PBSince.affects(reissueTx) shouldBe true
      PBSince.affects(burnTx) shouldBe true
    }

    "DataTransaction" in {
      val data           = IntegerDataEntry("key", 0)
      val dataTxUnsigned = DataTransaction.create(TxVersion.V2, account.publicKey, Seq(data), DataTxFee, now, Proofs.empty).explicitGet()

      val (proofs, dataTxJson) = Post(routePath("/sign"), dataTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, dataTxUnsigned)
      }

      val dataTx    = dataTxUnsigned.copy(proofs = proofs)
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(dataTx)))

      Post(routePath("/broadcast"), dataTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe dataTxJson
      }

      decode(base64Str) shouldBe dataTx

      (dataTx.json() \ "chainId").asOpt[Byte].value shouldBe dataTx.chainId

      PBSince.affects(dataTx) shouldBe true
    }

    "ExchangeTransaction" in {
      val buyer     = TxHelpers.signer(0)
      val seller    = TxHelpers.signer(1)
      val assetPair = AssetPair(asset, Waves)

      val buyOrder =
        Order
          .buy(Order.V3, buyer, account.publicKey, assetPair, Order.MaxAmount / 2, 100L, now, now + Order.MaxLiveTime / 2, MinFee * 3)
          .explicitGet()
      val sellOrder =
        Order
          .sell(Order.V3, seller, account.publicKey, assetPair, Order.MaxAmount / 2, 100L, now, now + Order.MaxLiveTime / 2, MinFee * 3)
          .explicitGet()

      val exchangeTx =
        TxHelpers.exchange(buyOrder, sellOrder, account, 100, 100, MinFee * 3, MinFee * 3, MinFee * 3, now, TxVersion.V3)
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(exchangeTx)))

      Post(routePath("/broadcast"), exchangeTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe exchangeTx.json()
      }

      decode(base64Str) shouldBe exchangeTx

      (exchangeTx.json() \ "chainId").asOpt[Byte].value shouldBe exchangeTx.chainId

      PBSince.affects(exchangeTx) shouldBe true
    }

    "InvokeScriptTransaction" in {
      val dapp = TxHelpers.secondAddress
      val invokeScriptTxUnsigned = InvokeScriptTransaction
        .create(
          TxVersion.V2,
          account.publicKey,
          dapp,
          Some(FUNCTION_CALL(User("hello"), List(CONST_LONG(42L)))),
          Seq(InvokeScriptTransaction.Payment(100L, Waves)),
          InvokeScriptTxFee,
          feeAssetId = asset,
          now,
          Proofs.empty,
          dapp.chainId
        )
        .explicitGet()

      val (proofs, invokeScriptTxJson) = Post(routePath("/sign"), invokeScriptTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, invokeScriptTxUnsigned)
      }

      val invokeScriptTx = invokeScriptTxUnsigned.copy(proofs = proofs)
      val base64Str      = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(invokeScriptTx)))

      Post(routePath("/broadcast"), invokeScriptTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe invokeScriptTxJson
      }

      decode(base64Str) shouldBe invokeScriptTx

      (invokeScriptTx.json() \ "chainId").asOpt[Byte].value shouldBe invokeScriptTx.chainId

      PBSince.affects(invokeScriptTx) shouldBe true
    }

    "LeaseTransaction/LeaseCancelTransaction" in {
      val recipient       = TxHelpers.secondAddress
      val leaseTxUnsigned = LeaseTransaction.create(TxVersion.V3, account.publicKey, recipient, 100L, MinFee, now, Proofs.empty).explicitGet()

      val (leaseProofs, leaseTxJson) = Post(routePath("/sign"), leaseTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, leaseTxUnsigned)
      }

      val leaseTx = leaseTxUnsigned.copy(proofs = leaseProofs)

      Post(routePath("/broadcast"), leaseTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe leaseTxJson
      }

      val leaseCancelTxUnsigned =
        LeaseCancelTransaction.create(TxVersion.V3, account.publicKey, leaseTx.id(), MinFee, now, Proofs.empty).explicitGet()

      val (leaseCancelProofs, leaseCancelTxJson) = Post(routePath("/sign"), leaseCancelTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, leaseCancelTxUnsigned)
      }

      val leaseCancelTx = leaseCancelTxUnsigned.copy(proofs = leaseCancelProofs)

      Post(routePath("/broadcast"), leaseCancelTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe leaseCancelTxJson
      }

      val base64LeaseStr       = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(leaseTx)))
      val base64CancelLeaseStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(leaseCancelTx)))

      decode(base64LeaseStr) shouldBe leaseTx
      decode(base64CancelLeaseStr) shouldBe leaseCancelTx

      (leaseTx.json() \ "chainId").asOpt[Byte].value shouldBe leaseTx.chainId
      (leaseCancelTx.json() \ "chainId").asOpt[Byte].value shouldBe leaseCancelTx.chainId

      PBSince.affects(leaseTx) shouldBe true
      PBSince.affects(leaseCancelTx) shouldBe true
    }

    "TransferTransaction" in {
      val recipient = TxHelpers.secondAddress
      val transferTxUnsigned =
        TransferTransaction
          .create(TxVersion.V3, account.publicKey, recipient, asset, 100L, Waves, MinFee, ByteStr(attachment), now, Proofs.empty)
          .explicitGet()

      val (proofs, transferTxJson) = Post(routePath("/sign"), transferTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, transferTxUnsigned)
      }

      val transferTx = transferTxUnsigned.copy(proofs = proofs)
      val base64Str  = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(transferTx)))

      Post(routePath("/broadcast"), transferTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe transferTxJson
      }

      decode(base64Str) shouldBe transferTx

      (transferTx.json() \ "chainId").asOpt[Byte].value shouldBe transferTx.chainId

      PBSince.affects(transferTx) shouldBe true
    }

    "MassTransferTransaction" in {
      val transfers = (1 to 10).map { i =>
        ParsedTransfer(TxHelpers.signer(i).toAddress, TxNonNegativeAmount.unsafeFrom(100))
      }
      val attachment = Array.fill(TransferTransaction.MaxAttachmentSize)(1: Byte)

      val massTransferTxUnsigned =
        MassTransferTransaction
          .create(TxVersion.V2, account.publicKey, Waves, transfers, MassTransferTxFee, now, ByteStr(attachment), Proofs.empty)
          .explicitGet()

      val (proofs, massTransferTxJson) = Post(routePath("/sign"), massTransferTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, massTransferTxUnsigned)
      }

      val massTransferTx = massTransferTxUnsigned.copy(proofs = proofs)
      val base64Str      = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(massTransferTx)))

      Post(routePath("/broadcast"), massTransferTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe massTransferTxJson
      }

      decode(base64Str) shouldBe massTransferTx

      (massTransferTx.json() \ "chainId").asOpt[Byte].value shouldBe massTransferTx.chainId

      PBSince.affects(massTransferTx) shouldBe true
    }

    "SetScriptTransaction" in {
      val setScriptTxUnsigned =
        SetScriptTransaction.create(TxVersion.V2, account.publicKey, Some(script), SetScriptFee, now, Proofs.empty).explicitGet()

      val (proofs, setScriptTxJson) = Post(routePath("/sign"), setScriptTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, setScriptTxUnsigned)
      }

      val setScriptTx = setScriptTxUnsigned.copy(proofs = proofs)
      val base64Str   = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setScriptTx)))

      Post(routePath("/broadcast"), setScriptTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe setScriptTxJson
      }

      decode(base64Str) shouldBe setScriptTx

      (setScriptTx.json() \ "chainId").asOpt[Byte].value shouldBe setScriptTx.chainId
    }

    "SetAssetScriptTransaction" in {
      val setAssetScriptTxUnsigned =
        SetAssetScriptTransaction.create(TxVersion.V2, account.publicKey, asset, Some(script), SetAssetScriptFee, now, Proofs.empty).explicitGet()

      val (proofs, setAssetScriptTxJson) = Post(routePath("/sign"), setAssetScriptTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, setAssetScriptTxUnsigned)
      }

      val setAssetScriptTx = setAssetScriptTxUnsigned.copy(proofs = proofs)
      val base64Str        = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setAssetScriptTx)))

      Post(routePath("/broadcast"), setAssetScriptTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe setAssetScriptTxJson
      }

      decode(base64Str) shouldBe setAssetScriptTx

      PBSince.affects(setAssetScriptTx) shouldBe true
    }

    "SponsorshipTransaction" in {
      val sponsorshipTxUnsigned =
        SponsorFeeTransaction.create(TxVersion.V2, account.publicKey, asset, Some(100L), MinFee, now, Proofs.empty).explicitGet()

      val (proofs, sponsorshipTxJson) = Post(routePath("/sign"), sponsorshipTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, sponsorshipTxUnsigned)
      }

      val sponsorshipTx = sponsorshipTxUnsigned.copy(proofs = proofs)
      val base64Str     = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(sponsorshipTx)))

      Post(routePath("/broadcast"), sponsorshipTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe sponsorshipTx.json()
        responseAs[JsObject] shouldBe sponsorshipTxJson
      }

      decode(base64Str) shouldBe sponsorshipTx

      (sponsorshipTx.json() \ "chainId").asOpt[Byte].value shouldBe sponsorshipTx.chainId

      PBSince.affects(sponsorshipTx) shouldBe true
    }

    "UpdateAssetInfoTransaction" in {
      val updateAssetInfoTxUnsigned = UpdateAssetInfoTransaction
        .create(
          TxVersion.V1,
          account.publicKey,
          asset,
          "Test",
          "Test",
          ntpNow,
          MinFee,
          Waves,
          Proofs.empty
        )
        .explicitGet()

      val (proofs, updateAssetInfoTxJson) = Post(routePath("/sign"), updateAssetInfoTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, updateAssetInfoTxUnsigned)
      }

      val updateAssetInfoTx = updateAssetInfoTxUnsigned.copy(proofs = proofs)
      val base64Str         = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(updateAssetInfoTx)))

      Post(routePath("/broadcast"), updateAssetInfoTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe updateAssetInfoTx.json()
        responseAs[JsObject] shouldBe updateAssetInfoTxJson
      }

      decode(base64Str) shouldBe updateAssetInfoTx

      (updateAssetInfoTx.json() \ "chainId").asOpt[Byte].value shouldBe updateAssetInfoTx.chainId
      (updateAssetInfoTx.json() \ "version").as[Byte] shouldBe TxVersion.V1
    }

    def checkProofs(response: HttpResponse, tx: Versioned): (Proofs, JsObject) = {
      response.status shouldBe StatusCodes.OK

      (responseAs[JsObject] \ "version").as[Byte] shouldBe tx.version
      (responseAs[JsObject] \ "senderPublicKey").asOpt[String].value should not be empty

      val json   = responseAs[JsObject]
      val proofs = (json \ "proofs").as[Proofs]
      proofs.size shouldBe 1
      (proofs, json)
    }

    def decode(base64Str: String): Transaction = {
      PBTransactions.vanilla(PBSignedTransaction.parseFrom(Base64.decode(base64Str)), unsafe = true).explicitGet()
    }
  }
}

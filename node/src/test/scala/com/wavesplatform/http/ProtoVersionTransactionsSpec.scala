package com.wavesplatform.http

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import com.wavesplatform.TestWallet
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.{RouteTimeout, TransactionsApiRoute}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.utils.SharedSchedulerMixin
import com.wavesplatform.utx.UtxPool
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import play.api.libs.json.*

import scala.concurrent.duration.*

class ProtoVersionTransactionsSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with MockFactory
    with OptionValues
    with TestWallet
    with SharedSchedulerMixin {

  private val MinFee: Long            = (0.001 * Constants.UnitsInWave).toLong
  private val DataTxFee: Long         = 15000000
  private val InvokeScriptTxFee: Long = 15000000
  private val MassTransferTxFee: Long = 15000000
  private val SetScriptFee: Long      = (0.01 * Constants.UnitsInWave).toLong
  private val SetAssetScriptFee: Long = Constants.UnitsInWave

  private val Now: Long = ntpNow

  private val account: KeyPair = testWallet.generateNewAccount().get

  private val blockchain: Blockchain = mock[Blockchain]
  private val utx: UtxPool           = mock[UtxPool]

  private val transactionsApi = mock[CommonTransactionsApi]
  private val route: Route =
    TransactionsApiRoute(
      restAPISettings,
      transactionsApi,
      testWallet,
      blockchain,
      mock[() => SnapshotBlockchain],
      () => utx.size,
      DummyTransactionPublisher.accepting,
      ntpTime,
      new RouteTimeout(60.seconds)(sharedScheduler)
    ).route

  "Proto transactions should be able to broadcast " - {
    "CreateAliasTransaction" in {
      val alias = aliasGen.sample.get

      val aliasTxUnsigned = CreateAliasTransaction.create(TxVersion.V3, account.publicKey, alias.name, MinFee, Now, Proofs.empty).explicitGet()

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
          Now,
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
        .create(TxVersion.V3, account.publicKey, issueTx.asset, quantity, reissuable, MinIssueFee, Now, Proofs.empty)
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
        BurnTransaction.create(TxVersion.V3, account.publicKey, issueTx.asset, quantity, MinIssueFee, Now, Proofs.empty).explicitGet()

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

      issueTx.isProtobufVersion shouldBe true
      reissueTx.isProtobufVersion shouldBe true
      burnTx.isProtobufVersion shouldBe true
    }

    "DataTransaction" in {
      val data = dataEntryGen(10).sample.get

      val dataTxUnsigned = DataTransaction.create(TxVersion.V2, account.publicKey, Seq(data), DataTxFee, Now, Proofs.empty).explicitGet()

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

      dataTx.isProtobufVersion shouldBe true
    }

    "ExchangeTransaction" in {
      val buyer     = accountGen.sample.get
      val seller    = accountGen.sample.get
      val assetPair = assetPairGen.sample.get

      val buyOrder =
        Order.buy(Order.V3, buyer, account.publicKey, assetPair, Order.MaxAmount / 2, 100L, Now, Now + Order.MaxLiveTime, MinFee * 3).explicitGet()
      val sellOrder =
        Order.sell(Order.V3, seller, account.publicKey, assetPair, Order.MaxAmount / 2, 100L, Now, Now + Order.MaxLiveTime, MinFee * 3).explicitGet()

      val exchangeTx =
        ExchangeTransaction
          .signed(TxVersion.V3, account.privateKey, buyOrder, sellOrder, 100, 100, MinFee * 3, MinFee * 3, MinFee * 3, Now)
          .explicitGet()
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(exchangeTx)))

      Post(routePath("/broadcast"), exchangeTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe exchangeTx.json()
      }

      decode(base64Str) shouldBe exchangeTx

      (exchangeTx.json() \ "chainId").asOpt[Byte].value shouldBe exchangeTx.chainId

      exchangeTx.isProtobufVersion shouldBe true
    }

    "InvokeScriptTransaction" in {
      val dapp       = accountOrAliasGen.sample.get
      val feeAssetId = bytes32gen.map(ByteStr(_)).sample.get

      val invokeScriptTxUnsigned = InvokeScriptTransaction
        .create(
          TxVersion.V2,
          account.publicKey,
          dapp,
          Some(FUNCTION_CALL(User("hello"), List(CONST_LONG(42L)))),
          Seq(InvokeScriptTransaction.Payment(100L, Asset.Waves)),
          InvokeScriptTxFee,
          IssuedAsset(feeAssetId),
          Now,
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

      invokeScriptTx.isProtobufVersion shouldBe true
    }

    "LeaseTransaction/LeaseCancelTransaction" in {
      val recipient = accountOrAliasGen.sample.get

      val leaseTxUnsigned = LeaseTransaction.create(TxVersion.V3, account.publicKey, recipient, 100L, MinFee, Now, Proofs.empty).explicitGet()

      val (leaseProofs, leaseTxJson) = Post(routePath("/sign"), leaseTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, leaseTxUnsigned)
      }

      val leaseTx = leaseTxUnsigned.copy(proofs = leaseProofs)

      Post(routePath("/broadcast"), leaseTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe leaseTxJson
      }

      val leaseCancelTxUnsigned =
        LeaseCancelTransaction.create(TxVersion.V3, account.publicKey, leaseTx.id(), MinFee, Now, Proofs.empty).explicitGet()

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

      leaseTx.isProtobufVersion shouldBe true
      leaseCancelTx.isProtobufVersion shouldBe true
    }

    "TransferTransaction" in {
      val recipient  = accountOrAliasGen.sample.get
      val asset      = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)
      val attachment = genBoundedBytes(0, TransferTransaction.MaxAttachmentSize).sample.get

      val transferTxUnsigned =
        TransferTransaction
          .create(TxVersion.V3, account.publicKey, recipient, asset, 100L, Asset.Waves, MinFee, ByteStr(attachment), Now, Proofs.empty)
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

      transferTx.isProtobufVersion shouldBe true
    }

    "MassTransferTransaction" in {
      val transfers =
        Gen.listOfN(10, accountOrAliasGen).map(accounts => accounts.map(ParsedTransfer(_, TxNonNegativeAmount.unsafeFrom(100)))).sample.get
      val attachment = genBoundedBytes(0, TransferTransaction.MaxAttachmentSize).sample.get

      val massTransferTxUnsigned =
        MassTransferTransaction
          .create(TxVersion.V2, account.publicKey, Asset.Waves, transfers, MassTransferTxFee, Now, ByteStr(attachment), Proofs.empty)
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

      massTransferTx.isProtobufVersion shouldBe true
    }

    "SetScriptTransaction" in {
      val script = scriptGen.sample.get

      val setScriptTxUnsigned =
        SetScriptTransaction.create(TxVersion.V2, account.publicKey, Some(script), SetScriptFee, Now, Proofs.empty).explicitGet()

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
      val asset  = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)
      val script = scriptGen.sample.get

      val setAssetScriptTxUnsigned =
        SetAssetScriptTransaction.create(TxVersion.V2, account.publicKey, asset, Some(script), SetAssetScriptFee, Now, Proofs.empty).explicitGet()

      val (proofs, setAssetScriptTxJson) = Post(routePath("/sign"), setAssetScriptTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response, setAssetScriptTxUnsigned)
      }

      val setAssetScriptTx = setAssetScriptTxUnsigned.copy(proofs = proofs)
      val base64Str        = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setAssetScriptTx)))

      Post(routePath("/broadcast"), setAssetScriptTx.json()) ~> ApiKeyHeader ~> route ~> check {
        responseAs[JsObject] shouldBe setAssetScriptTxJson
      }

      decode(base64Str) shouldBe setAssetScriptTx

      setAssetScriptTx.isProtobufVersion shouldBe true
    }

    "SponsorshipTransaction" in {
      val asset = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)

      val sponsorshipTxUnsigned =
        SponsorFeeTransaction.create(TxVersion.V2, account.publicKey, asset, Some(100L), MinFee, Now, Proofs.empty).explicitGet()

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

      sponsorshipTx.isProtobufVersion shouldBe true
    }

    "UpdateAssetInfoTransaction" in {
      val asset = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)

      val updateAssetInfoTxUnsigned = UpdateAssetInfoTransaction
        .create(
          TxVersion.V1,
          account.publicKey,
          asset.id,
          "Test",
          "Test",
          ntpNow,
          MinFee,
          Asset.Waves,
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

    def checkProofs(response: HttpResponse, tx: VersionedTransaction): (Proofs, JsObject) = {
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

package com.wavesplatform.http

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.TransactionsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.settings.{Constants, WalletSettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, DataTransaction, Proofs, Transaction, TxVersion}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers
import play.api.libs.json.JsObject

class ProtoVersionTransactionsSpec extends RouteSpec("/transactions") with RestAPISettingsHelper with MockFactory with TransactionGen with Matchers {
  import com.wavesplatform.http.ApiMarshallers._

  private val MinFee: Long            = (0.001 * Constants.UnitsInWave).toLong
  private val DataTxFee: Long         = 15000000
  private val InvokeScriptTxFee: Long = 15000000
  private val MassTransferTxFee: Long = 15000000
  private val SetScriptFee: Long      = (0.01 * Constants.UnitsInWave).toLong
  private val SetAssetScriptFee: Long = Constants.UnitsInWave

  private val Now: Long = ntpNow

  private val wallet: Wallet   = Wallet(WalletSettings(None, Some("qwerty"), None))
  private val account: KeyPair = wallet.generateNewAccount().get

  private val blockchain: Blockchain                   = mock[Blockchain]
  private val utx: UtxPool                             = mock[UtxPool]
  private val utxPoolSynchronizer: UtxPoolSynchronizer = mock[UtxPoolSynchronizer]

  private val route: Route = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

  "all txs" - {
    "CreateAliasTransaction" in {
      val alias = aliasGen.sample.get

      val aliasTxUnsigned = CreateAliasTransaction.create(TxVersion.V3, account, alias, MinFee, Now, Proofs.empty).explicitGet()

      val proofs = Post(routePath("/sign"), aliasTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val aliasTx  = aliasTxUnsigned.copy(proofs = proofs)
      val base64Tx = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(aliasTx)))

      (aliasTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
      decode(base64Tx) shouldBe aliasTx
    }

    "IssueTransaction/ReissueTransaction/BurnTransaction" in {
      val name        = "Test asset".getBytes("utf-8")
      val description = "Test description".getBytes("utf-8")
      val quantity    = 1000
      val decimals    = 2.toByte
      val reissuable  = true

      val issueTxUnsigned = IssueTransaction
        .create(TxVersion.V3, account, name, description, quantity, decimals, reissuable, script = None, MinIssueFee, Now, Proofs.empty)
        .explicitGet()

      val issueProofs = Post(routePath("/sign"), issueTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val issueTx        = issueTxUnsigned.copy(proofs = issueProofs)
      val base64IssueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(issueTx)))

      val reissueTxUnsigned = ReissueTransaction
        .create(TxVersion.V3, account, IssuedAsset(issueTx.assetId), quantity, reissuable, MinIssueFee, Now, Proofs.empty)
        .explicitGet()

      val reissueProofs = Post(routePath("/sign"), reissueTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val reissueTx        = reissueTxUnsigned.copy(proofs = reissueProofs)
      val base64reissueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(reissueTx)))

      val burnTxUnsigned =
        BurnTransaction.create(TxVersion.V3, account, IssuedAsset(issueTx.assetId), quantity, MinIssueFee, Now, Proofs.empty).explicitGet()

      val burnProofs = Post(routePath("/sign"), burnTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val burnTx        = burnTxUnsigned.copy(proofs = burnProofs)
      val base64BurnStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(burnTx)))

      decode(base64IssueStr) shouldBe issueTx
      decode(base64reissueStr) shouldBe reissueTx
      decode(base64BurnStr) shouldBe burnTx

      (issueTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
      (reissueTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
      (burnTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "DataTransaction" in {
      val data = dataEntryGen(10).sample.get

      val dataTxUnsigned = DataTransaction.create(TxVersion.V2, account, Seq(data), DataTxFee, Now, Proofs.empty).explicitGet()

      val proofs = Post(routePath("/sign"), dataTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val dataTx    = dataTxUnsigned.copy(proofs = proofs)
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(dataTx)))

      decode(base64Str) shouldBe dataTx

      (dataTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "ExchangeTransaction" in {
      val buyer     = accountGen.sample.get
      val seller    = accountGen.sample.get
      val assetPair = assetPairGen.sample.get

      val buyOrder  = Order.buy(Order.V3, buyer, account, assetPair, Order.MaxAmount / 2, 100, Now, Now + Order.MaxLiveTime, MinFee * 3)
      val sellOrder = Order.sell(Order.V3, seller, account, assetPair, Order.MaxAmount / 2, 100, Now, Now + Order.MaxLiveTime, MinFee * 3)

      val exchangeTx =
        ExchangeTransaction.signed(TxVersion.V3, account, buyOrder, sellOrder, 100, 100, MinFee * 3, MinFee * 3, MinFee * 3, Now).explicitGet()
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(exchangeTx)))

      decode(base64Str) shouldBe exchangeTx

      (exchangeTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "InvokeScriptTransaction" in {
      val dapp       = accountOrAliasGen.sample.get
      val feeAssetId = bytes32gen.map(ByteStr(_)).sample.get

      val invokeScriptTxUnsigned = InvokeScriptTransaction
        .create(
          TxVersion.V2,
          account,
          dapp,
          Some(FUNCTION_CALL(User("hello"), List(CONST_LONG(42L)))),
          Seq(Payment(100, Asset.Waves)),
          InvokeScriptTxFee,
          IssuedAsset(feeAssetId),
          Now,
          Proofs.empty
        )
        .explicitGet()

      val proofs = Post(routePath("/sign"), invokeScriptTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val invokeScriptTx = invokeScriptTxUnsigned.copy(proofs = proofs)
      val base64Str      = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(invokeScriptTx)))

      decode(base64Str) shouldBe invokeScriptTx

      (invokeScriptTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "LeaseTransaction/LeaseCancelTransaction" in {
      val recipient = accountOrAliasGen.sample.get

      val leaseTxUnsigned = LeaseTransaction.create(TxVersion.V3, account, recipient, 100, MinFee, Now, Proofs.empty).explicitGet()

      val leaseProofs = Post(routePath("/sign"), leaseTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val leaseTx = leaseTxUnsigned.copy(proofs = leaseProofs)

      val leaseCancelTxUnsigned = LeaseCancelTransaction.create(TxVersion.V3, account, leaseTx.id(), MinFee, Now, Proofs.empty).explicitGet()

      val leaseCancelProofs = Post(routePath("/sign"), leaseCancelTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val leaseCancelTx = leaseCancelTxUnsigned.copy(proofs = leaseCancelProofs)

      val base64LeaseStr       = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(leaseTx)))
      val base64CancelLeaseStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(leaseCancelTx)))

      decode(base64LeaseStr) shouldBe leaseTx
      decode(base64CancelLeaseStr) shouldBe leaseCancelTx

      (leaseTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
      (leaseCancelTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "TransferTransaction" in {
      val recipient  = accountOrAliasGen.sample.get
      val asset      = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)
      val attachment = genBoundedBytes(0, TransferTransaction.MaxAttachmentSize).sample.get

      val transferTxUnsigned =
        TransferTransaction.create(TxVersion.V3, account, recipient, asset, 100, Asset.Waves, MinFee, attachment, Now, Proofs.empty).explicitGet()

      val proofs = Post(routePath("/sign"), transferTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val transferTx = transferTxUnsigned.copy(proofs = proofs)
      val base64Str  = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(transferTx)))

      decode(base64Str) shouldBe transferTx

      (transferTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "MassTransferTransaction" in {
      val transfers  = Gen.listOfN(10, accountOrAliasGen).map(accounts => accounts.map(ParsedTransfer(_, 100))).sample.get
      val attachment = genBoundedBytes(0, TransferTransaction.MaxAttachmentSize).sample.get

      val massTransferTxUnsigned =
        MassTransferTransaction.create(TxVersion.V2, account, Asset.Waves, transfers, MassTransferTxFee, Now, attachment, Proofs.empty).explicitGet()

      val proofs = Post(routePath("/sign"), massTransferTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val massTransferTx = massTransferTxUnsigned.copy(proofs = proofs)
      val base64Str      = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(massTransferTx)))

      decode(base64Str) shouldBe massTransferTx

      (massTransferTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "SetScriptTransaction" in {
      val script = scriptGen.sample.get

      val setScriptTxUnsigned = SetScriptTransaction.create(TxVersion.V2, account, Some(script), SetScriptFee, Now, Proofs.empty).explicitGet()

      val proofs = Post(routePath("/sign"), setScriptTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val setScriptTx = setScriptTxUnsigned.copy(proofs = proofs)
      val base64Str   = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setScriptTx)))

      decode(base64Str) shouldBe setScriptTx

      (setScriptTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    "SetAssetScriptTransaction" in {
      val asset  = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)
      val script = scriptGen.sample.get

      val setAssetScriptTxUnsigned =
        SetAssetScriptTransaction.create(TxVersion.V2, account, asset, Some(script), SetAssetScriptFee, Now, Proofs.empty).explicitGet()

      val proofs = Post(routePath("/sign"), setAssetScriptTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val setAssetScriptTx = setAssetScriptTxUnsigned.copy(proofs = proofs)
      val base64Str        = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setAssetScriptTx)))

      decode(base64Str) shouldBe setAssetScriptTx
    }

    "SponsorshipTransaction" in {
      val asset = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)

      val sponsorshipTxUnsigned = SponsorFeeTransaction.selfSigned(TxVersion.V2, account, asset, Some(100), MinFee, Now).explicitGet()

      val proofs = Post(routePath("/sign"), sponsorshipTxUnsigned.json()) ~> ApiKeyHeader ~> route ~> check {
        checkProofs(response)
      }

      val sponsorshipTx = sponsorshipTxUnsigned.copy(proofs = proofs)
      val base64Str     = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(sponsorshipTx)))

      decode(base64Str) shouldBe sponsorshipTx

      (sponsorshipTx.json() \ "chainId").asOpt[Byte] shouldBe 'defined
    }

    def checkProofs(response: HttpResponse): Proofs = {
      response.status shouldBe StatusCodes.OK
      val r = (responseAs[JsObject] \ "proofs").as[Proofs]
      r.size shouldBe 1
      r
    }

    def decode(base64Str: String): Transaction = {
      PBTransactions.vanilla(PBSignedTransaction.parseFrom(Base64.decode(base64Str))).explicitGet()
    }
  }
}

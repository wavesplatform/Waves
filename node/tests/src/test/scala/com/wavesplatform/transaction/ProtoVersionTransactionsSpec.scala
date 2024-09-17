package com.wavesplatform.transaction

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.settings.Constants
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.utils.Signed
import org.scalacheck.Gen

class ProtoVersionTransactionsSpec extends FreeSpec {

  val MinFee: Long            = (0.001 * Constants.UnitsInWave).toLong
  val DataTxFee: Long         = 15000000
  val InvokeScriptTxFee: Long = 15000000
  val MassTransferTxFee: Long = 15000000
  val SetScriptFee: Long      = (0.01 * Constants.UnitsInWave).toLong
  val SetAssetScriptFee: Long = Constants.UnitsInWave

  val Now: Long = ntpNow

  val Account: KeyPair = accountGen.sample.get

  "all txs" - {
    "CreateAliasTransaction" in {
      val alias = aliasGen.sample.get

      val aliasTx  = CreateAliasTransaction.selfSigned(TxVersion.V3, Account, alias.name, MinFee, Now).explicitGet()
      val base64Tx = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(aliasTx)))

      decode(base64Tx) shouldBe aliasTx
    }

    "IssueTransaction/ReissueTransaction/BurnTransaction" in {
      val name        = "Test asset"
      val description = "Test description"
      val quantity    = 1000
      val decimals    = 2.toByte
      val reissuable  = true

      val issueTx = IssueTransaction
        .selfSigned(
          TxVersion.V3,
          Account,
          name,
          description,
          quantity,
          decimals,
          reissuable,
          script = None,
          MinIssueFee,
          Now
        )
        .explicitGet()
      val base64IssueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(issueTx)))

      val reissueTx = ReissueTransaction
        .selfSigned(TxVersion.V3, Account, issueTx.asset, quantity, reissuable, MinIssueFee, Now)
        .explicitGet()
      val base64reissueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(reissueTx)))

      val burnTx        = BurnTransaction.selfSigned(TxVersion.V3, Account, issueTx.asset, quantity, MinIssueFee, Now).explicitGet()
      val base64BurnStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(burnTx)))

      decode(base64IssueStr) shouldBe issueTx
      decode(base64reissueStr) shouldBe reissueTx
      decode(base64BurnStr) shouldBe burnTx
    }

    "DataTransaction" in {
      val data = dataEntryGen(10).sample.get

      val dataTx    = DataTransaction.selfSigned(TxVersion.V2, Account, Seq(data), DataTxFee, Now).explicitGet()
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(dataTx)))

      decode(base64Str) shouldBe dataTx
    }

    "ExchangeTransaction" in {
      val buyer     = accountGen.sample.get
      val seller    = accountGen.sample.get
      val assetPair = assetPairGen.sample.get

      val buyOrder =
        Order.buy(Order.V3, buyer, Account.publicKey, assetPair, Order.MaxAmount / 2, 100, Now, Now + Order.MaxLiveTime / 2, MinFee * 3).explicitGet()
      val sellOrder =
        Order.sell(Order.V3, seller, Account.publicKey, assetPair, Order.MaxAmount / 2, 100, Now, Now + Order.MaxLiveTime / 2, MinFee * 3).explicitGet()

      val exchangeTx =
        ExchangeTransaction
          .signed(TxVersion.V3, Account.privateKey, buyOrder, sellOrder, 100, 100, MinFee * 3, MinFee * 3, MinFee * 3, Now)
          .explicitGet()
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(exchangeTx)))

      decode(base64Str) shouldBe exchangeTx
    }

    "InvokeScriptTransaction" in {
      val dapp       = accountOrAliasGen.sample.get
      val feeAssetId = bytes32gen.map(ByteStr(_)).sample.get

      val invokeScriptTx = Signed.invokeScript(
        TxVersion.V2,
        Account,
        dapp,
        Some(FUNCTION_CALL(User("hello"), List(CONST_LONG(42L)))),
        Seq(Payment(100, Asset.Waves)),
        InvokeScriptTxFee,
        IssuedAsset(feeAssetId),
        Now
      )
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(invokeScriptTx)))

      decode(base64Str) shouldBe invokeScriptTx
    }

    "LeaseTransaction/LeaseCancelTransaction" in {
      val recipient = accountOrAliasGen.sample.get

      val leaseTx = LeaseTransaction.selfSigned(TxVersion.V3, Account, recipient, 100, MinFee, Now).explicitGet()
      val leaseCancelTx =
        LeaseCancelTransaction
          .selfSigned(TxVersion.V3, Account, leaseTx.id(), MinFee, Now)
          .explicitGet()
      val base64LeaseStr       = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(leaseTx)))
      val base64CancelLeaseStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(leaseCancelTx)))

      decode(base64LeaseStr) shouldBe leaseTx
      decode(base64CancelLeaseStr) shouldBe leaseCancelTx
    }

    "TransferTransaction" in {
      val recipient  = accountOrAliasGen.sample.get
      val asset      = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)
      val attachment = genBoundedBytes(0, TransferTransaction.MaxAttachmentSize).sample.get

      val transferTx =
        TransferTransaction
          .selfSigned(TxVersion.V3, Account, recipient, asset, 100, Asset.Waves, MinFee, ByteStr(attachment), Now)
          .explicitGet()

      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(transferTx)))

      decode(base64Str) shouldBe transferTx
    }

    "MassTransferTransaction" in {
      val transfers =
        Gen.listOfN(10, accountOrAliasGen).map(accounts => accounts.map(ParsedTransfer(_, TxNonNegativeAmount.unsafeFrom(100)))).sample.get
      val attachment = genBoundedBytes(0, TransferTransaction.MaxAttachmentSize).sample.get

      val massTransferTx =
        MassTransferTransaction
          .selfSigned(TxVersion.V2, Account, Asset.Waves, transfers, MassTransferTxFee, Now, ByteStr(attachment))
          .explicitGet()
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(massTransferTx)))

      decode(base64Str) shouldBe massTransferTx
    }

    "SetScriptTransaction" in {
      val script = scriptGen.sample.get

      val setScriptTx = SetScriptTransaction.selfSigned(TxVersion.V2, Account, Some(script), SetScriptFee, Now).explicitGet()
      val base64Str   = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setScriptTx)))

      decode(base64Str) shouldBe setScriptTx
    }

    "SetAssetScriptTransaction" in {
      val asset  = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)
      val script = scriptGen.sample.get

      val setAssetScriptTx = SetAssetScriptTransaction.selfSigned(TxVersion.V2, Account, asset, Some(script), SetAssetScriptFee, Now).explicitGet()
      val base64Str        = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setAssetScriptTx)))

      decode(base64Str) shouldBe setAssetScriptTx
    }

    "SponsorshipTransaction" in {
      val asset = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)

      val sponsorshipTx = SponsorFeeTransaction.selfSigned(TxVersion.V2, Account, asset, Some(100), MinFee, Now).explicitGet()
      val base64Str     = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(sponsorshipTx)))

      decode(base64Str) shouldBe sponsorshipTx
    }

    def decode(base64Str: String): Transaction = {
      PBTransactions.vanilla(PBSignedTransaction.parseFrom(Base64.decode(base64Str)), unsafe = false).explicitGet()
    }
  }
}

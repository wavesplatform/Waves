package com.wavesplatform.transaction

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.Height
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
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

      val aliasTx  = TxHelpers.createAlias(name = alias.name, sender = Account, fee = MinFee, version = TxVersion.V3)
      val base64Tx = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(aliasTx)))

      decode(base64Tx) shouldBe aliasTx
    }

    "IssueTransaction/ReissueTransaction/BurnTransaction" in {
      val name        = "Test asset"
      val description = "Test description"
      val quantity    = 1000
      val decimals    = 2.toByte
      val reissuable  = true

      val issueTx = TxHelpers.issue(issuer = Account, amount = quantity, decimals = decimals, name = name, description = description, fee = MinIssueFee, script = None, reissuable = reissuable, version = TxVersion.V3)
      val base64IssueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(issueTx)))

      val reissueTx = TxHelpers.reissue(asset = issueTx.asset, sender = Account, amount = quantity, reissuable = reissuable, fee = MinIssueFee, version = TxVersion.V3)
      val base64reissueStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(reissueTx)))

      val burnTx        = TxHelpers.burn(asset = issueTx.asset, amount = quantity, sender = Account, fee = MinIssueFee, version = TxVersion.V3)
      val base64BurnStr = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(burnTx)))

      decode(base64IssueStr) shouldBe issueTx
      decode(base64reissueStr) shouldBe reissueTx
      decode(base64BurnStr) shouldBe burnTx
    }

    "DataTransaction" in {
      val data = dataEntryGen(10).sample.get

      val dataTx    = TxHelpers.dataV2(account = Account, entries = Seq(data), fee = DataTxFee)
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
        Order
          .sell(Order.V3, seller, Account.publicKey, assetPair, Order.MaxAmount / 2, 100, Now, Now + Order.MaxLiveTime / 2, MinFee * 3)
          .explicitGet()

      val exchangeTx =
        TxHelpers.exchange(buyOrder, sellOrder, Account, 100, 100, MinFee * 3, MinFee * 3, MinFee * 3, Now, TxVersion.V3)
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(exchangeTx)))

      decode(base64Str) shouldBe exchangeTx
    }

    "InvokeScriptTransaction" in {
      val dapp       = accountOrAliasGen.sample.get
      val feeAssetId = bytes32gen.map(ByteStr(_)).sample.get

      val invokeScriptTx = InvokeScriptTransaction.create(
        TxVersion.V2,
        Account.publicKey,
        dapp,
        Some(FUNCTION_CALL(User("hello"), List(CONST_LONG(42L)))),
        Seq(Payment(100, Asset.Waves)),
        InvokeScriptTxFee,
        IssuedAsset(feeAssetId),
        Now
      ).map(_.signWith(Account.privateKey)).explicitGet()
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(invokeScriptTx)))

      decode(base64Str) shouldBe invokeScriptTx
    }

    "LeaseTransaction/LeaseCancelTransaction" in {
      val recipient = accountOrAliasGen.sample.get

      val leaseTx = LeaseTransaction.create(TxVersion.V3, Account.publicKey, recipient, 100, MinFee, Now, Proofs.empty).map(_.signWith(Account.privateKey)).explicitGet()
      val leaseCancelTx =
        TxHelpers.leaseCancel(leaseId = leaseTx.id(), sender = Account, fee = MinFee, version = TxVersion.V3)
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
          .create(TxVersion.V3, Account.publicKey, recipient, asset, 100, Asset.Waves, MinFee, ByteStr(attachment), Now, Proofs.empty)
          .map(_.signWith(Account.privateKey))
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
          .create(TxVersion.V2, Account.publicKey, Asset.Waves, transfers, MassTransferTxFee, Now, ByteStr(attachment), Proofs.empty)
          .map(_.signWith(Account.privateKey))
          .explicitGet()
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(massTransferTx)))

      decode(base64Str) shouldBe massTransferTx
    }

    "SetScriptTransaction" in {
      val script = scriptGen.sample.get

      val setScriptTx = SetScriptTransaction.create(TxVersion.V2, Account.publicKey, Some(script), SetScriptFee, Now, Proofs.empty).map(_.signWith(Account.privateKey)).explicitGet()
      val base64Str   = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setScriptTx)))

      decode(base64Str) shouldBe setScriptTx
    }

    "SetAssetScriptTransaction" in {
      val asset  = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)
      val script = scriptGen.sample.get

      val setAssetScriptTx = SetAssetScriptTransaction.create(TxVersion.V2, Account.publicKey, asset, Some(script), SetAssetScriptFee, Now, Proofs.empty).map(_.signWith(Account.privateKey)).explicitGet()
      val base64Str        = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(setAssetScriptTx)))

      decode(base64Str) shouldBe setAssetScriptTx
    }

    "SponsorshipTransaction" in {
      val asset = IssuedAsset(bytes32gen.map(ByteStr(_)).sample.get)

      val sponsorshipTx = SponsorFeeTransaction.create(TxVersion.V2, Account.publicKey, asset, Some(100), MinFee, Now, Proofs.empty).map(_.signWith(Account.privateKey)).explicitGet()
      val base64Str     = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(sponsorshipTx)))

      decode(base64Str) shouldBe sponsorshipTx
    }

    "CommitToGenerationTransaction" in {
      val tx        = TxHelpers.commitToGeneration(Height(3001), Account, Now, MinFee)
      val base64Str = Base64.encode(PBUtils.encodeDeterministic(PBTransactions.protobuf(tx)))
      decode(base64Str) shouldBe tx
    }

    def decode(base64Str: String): Transaction = {
      PBTransactions.vanilla(PBSignedTransaction.parseFrom(Base64.decode(base64Str)), unsafe = false).explicitGet()
    }
  }
}

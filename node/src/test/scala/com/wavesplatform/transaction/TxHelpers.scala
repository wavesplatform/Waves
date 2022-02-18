package com.wavesplatform.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit, ScriptExtraFee}
import com.wavesplatform.state.{DataEntry, StringDataEntry}
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, SetAssetScriptTransaction, SponsorFeeTransaction, UpdateAssetInfoTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}

object TxHelpers {
  def signer(i: Int): KeyPair  = KeyPair(Ints.toByteArray(i))
  def address(i: Int): Address = signer(i).toAddress

  def defaultSigner: KeyPair  = signer(0)
  def defaultAddress: Address = defaultSigner.toAddress
  def secondSigner: KeyPair   = signer(1)
  def secondAddress: Address  = secondSigner.toAddress

  private[this] var lastTimestamp = System.currentTimeMillis()
  def timestamp: Long = {
    lastTimestamp += 1
    lastTimestamp
  }

  def genesis(address: Address, amount: Long = ENOUGH_AMT, timestamp: TxTimestamp = timestamp): GenesisTransaction =
    GenesisTransaction.create(address, amount, timestamp).explicitGet()

  def payment(from: KeyPair = defaultSigner,
              to: Address = secondAddress,
              amount: Long = 1.waves): PaymentTransaction =
    PaymentTransaction.create(from, to, amount, TestValues.fee, timestamp).explicitGet()

  def transfer(from: KeyPair = defaultSigner,
               to: AddressOrAlias = secondAddress,
               amount: Long = 1.waves,
               asset: Asset = Waves,
               fee: Long = TestValues.fee,
               feeAsset: Asset = Waves,
               attachment: ByteStr = ByteStr.empty,
               timestamp: TxTimestamp = timestamp,
               version: Byte = TxVersion.V2): TransferTransaction =
    TransferTransaction.selfSigned(version, from, to, asset, amount, feeAsset, fee, attachment, timestamp).explicitGet()

  def transferUnsigned(from: KeyPair = defaultSigner,
                       to: AddressOrAlias = secondAddress,
                       amount: Long = 1.waves,
                       asset: Asset = Waves,
                       fee: Long = TestValues.fee,
                       feeAsset: Asset = Waves,
                       version: Byte = TxVersion.V2): TransferTransaction =
    TransferTransaction(version, from.publicKey, to, asset, amount, feeAsset, fee, ByteStr.empty, timestamp, Proofs.empty, to.chainId)

  def massTransfer(from: KeyPair = defaultSigner,
                   to: Seq[ParsedTransfer] = Seq(ParsedTransfer(secondAddress, 1.waves)),
                   asset: Asset = Waves,
                   fee: Long = TestValues.fee,
                   version: Byte = TxVersion.V2,
                   chainId: Byte = AddressScheme.current.chainId): MassTransferTransaction =
    MassTransferTransaction.selfSigned(version, from, asset, to, fee, timestamp, ByteStr.empty, chainId).explicitGet()

  def issue(issuer: KeyPair = defaultSigner,
            amount: Long = 1000,
            decimals: Byte = 0,
            name: String = "test",
            description: String = "description",
            fee: Long = 1.waves,
            script: Option[Script] = None,
            reissuable: Boolean = true,
            timestamp: TxTimestamp = timestamp,
            version: TxVersion = TxVersion.V2,
            chainId: Byte = AddressScheme.current.chainId): IssueTransaction =
    IssueTransaction
      .selfSigned(version, issuer, name, description, amount, decimals, reissuable, script, fee, timestamp, chainId)
      .explicitGet()

  def reissue(asset: IssuedAsset,
              sender: KeyPair = defaultSigner,
              amount: Long = 1000,
              fee: TxAmount = TestValues.fee,
              version: TxVersion = TxVersion.V2,
              chainId: Byte = AddressScheme.current.chainId): ReissueTransaction =
    ReissueTransaction
      .selfSigned(version, sender, asset, amount, reissuable = true, fee, timestamp, chainId)
      .explicitGet()

  def burn(asset: IssuedAsset,
           amount: Long = 1,
           sender: KeyPair = defaultSigner,
           fee: TxAmount = TestValues.fee,
           version: TxVersion = TxVersion.V3,
           chainId: Byte = AddressScheme.current.chainId): BurnTransaction =
    BurnTransaction.selfSigned(version, sender, asset, amount, fee, timestamp, chainId).explicitGet()

  def updateAssetInfo(assetId: ByteStr,
                      name: String = "updated_name",
                      desc: String = "updated_desc",
                      sender: KeyPair = defaultSigner,
                      version: TxVersion = TxVersion.V1,
                      chainId: Byte = AddressScheme.current.chainId): UpdateAssetInfoTransaction =
    UpdateAssetInfoTransaction.selfSigned(version, sender, assetId, name, desc, timestamp, TestValues.fee, Waves, chainId).explicitGet()

  def dataEntry(account: KeyPair, value: DataEntry[_]): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V1, account, Seq(value), TestValues.fee * 3, timestamp).explicitGet()

  def dataSingle(account: KeyPair = defaultSigner, key: String = "test", value: String = "test"): DataTransaction =
    data(account, Seq(StringDataEntry(key, value)))

  def data(account: KeyPair, entries: Seq[DataEntry[_]], fee: TxAmount = TestValues.fee * 3, version: TxVersion = TxVersion.V1): DataTransaction =
    DataTransaction.selfSigned(version, account, entries, fee, timestamp).explicitGet()

  def dataV2(account: KeyPair, entries: Seq[DataEntry[_]], fee: Long = TestValues.fee * 3, chainId: Byte = AddressScheme.current.chainId): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V2, account, entries, fee, timestamp, chainId).explicitGet()

  def orderV3(orderType: OrderType, asset: Asset, feeAsset: Asset): Order = {
    order(orderType, asset, Waves, feeAsset)
  }

  def orderV3(orderType: OrderType, asset: Asset): Order =
    orderV3(orderType, asset, Waves)

  def order(
      orderType: OrderType,
      amountAsset: Asset,
      priceAsset: Asset,
      feeAsset: Asset = Waves,
      amount: TxAmount = 1L,
      price: TxAmount = 1L,
      fee: TxAmount = 1L,
      sender: KeyPair = defaultSigner,
      matcher: KeyPair = defaultSigner,
      version: TxVersion = TxVersion.V3
  ): Order = {
    Order.selfSigned(
      version,
      sender,
      matcher.publicKey,
      AssetPair(amountAsset, priceAsset),
      orderType,
      amount,
      price,
      timestamp,
      timestamp + 100000,
      fee,
      feeAsset
    )
  }

  def exchange(order1: Order, order2: Order, matcher: KeyPair = defaultSigner, version: TxVersion = TxVersion.V2, chainId: Byte = AddressScheme.current.chainId): ExchangeTransaction = {
    ExchangeTransaction
      .signed(
        version,
        matcher.privateKey,
        order1,
        order2,
        order1.amount,
        order1.price,
        order1.matcherFee,
        order2.matcherFee,
        TestValues.fee,
        timestamp,
        chainId
      )
      .explicitGet()
  }

  def script(scriptText: String): Script = {
    val (script, _) = ScriptCompiler.compile(scriptText, ScriptEstimatorV3(fixOverflow = true)).explicitGet()
    script
  }

  def exprScript(version: StdLibVersion)(scriptText: String): ExprScriptImpl =
    script(s"""
              |{-# STDLIB_VERSION ${version.id} #-}
              |{-# CONTENT_TYPE EXPRESSION #-}
              |
              |$scriptText
              |""".stripMargin) match {
      case es: ExprScriptImpl => es
      case other              => throw new IllegalStateException(s"Not an expression: $other")
    }

  def setScript(acc: KeyPair, script: Script, fee: Long = TestValues.fee, version: TxVersion = TxVersion.V1, chainId: Byte = AddressScheme.current.chainId): SetScriptTransaction = {
    SetScriptTransaction.selfSigned(version, acc, Some(script), fee, timestamp, chainId).explicitGet()
  }

  def setAssetScript(acc: KeyPair,
                     asset: IssuedAsset,
                     script: Script,
                     fee: TxAmount = TestValues.fee,
                     timestamp: TxTimestamp = timestamp,
                     version: TxVersion = TxVersion.V1,
                     chainId: Byte = AddressScheme.current.chainId): SetAssetScriptTransaction = {
    SetAssetScriptTransaction.selfSigned(version, acc, asset, Some(script), fee, timestamp, chainId).explicitGet()
  }

  def invoke(
      dApp: AddressOrAlias,
      func: Option[String] = None,
      args: Seq[EXPR] = Nil,
      payments: Seq[Payment] = Nil,
      invoker: KeyPair = defaultSigner,
      fee: Long = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit,
      feeAssetId: Asset = Waves,
      version: TxVersion = TxVersion.V2
  ): InvokeScriptTransaction = {
    val fc = func.map(name => functionCall(name, args: _*))
    InvokeScriptTransaction.selfSigned(version, invoker, dApp, fc, payments, fee, feeAssetId, timestamp).explicitGet()
  }

  def functionCall(func: String, args: EXPR*): FUNCTION_CALL = {
    FUNCTION_CALL(FunctionHeader.User(func), args.toList)
  }

  def lease(sender: KeyPair = defaultSigner,
            recipient: AddressOrAlias = secondAddress,
            amount: TxAmount = 10.waves,
            fee: Long = TestValues.fee,
            timestamp: TxTimestamp = timestamp,
            version: TxVersion = TxVersion.V2): LeaseTransaction = {
    LeaseTransaction.selfSigned(version, sender, recipient, amount, fee, timestamp).explicitGet()
  }

  def leaseCancel(leaseId: ByteStr,
                  sender: KeyPair = defaultSigner,
                  fee: Long = TestValues.fee,
                  timestamp: TxTimestamp = timestamp,
                  version: TxVersion = TxVersion.V2,
                  chainId: Byte = AddressScheme.current.chainId): LeaseCancelTransaction = {
    LeaseCancelTransaction.selfSigned(version, sender, leaseId, fee, timestamp, chainId).explicitGet()
  }

  def sponsor(asset: IssuedAsset,
              minSponsoredAssetFee: Option[TxAmount] = Some(TestValues.fee),
              sender: KeyPair = defaultSigner,
              fee: TxAmount = TestValues.fee,
              version: TxVersion = TxVersion.V1,
              chainId: Byte = AddressScheme.current.chainId): SponsorFeeTransaction = {
    SponsorFeeTransaction.selfSigned(version, sender, asset, minSponsoredAssetFee, fee, timestamp, chainId).explicitGet()
  }

  def createAlias(name: String,
                  sender: KeyPair = defaultSigner,
                  fee: Long = TestValues.fee,
                  version: TxVersion = TxVersion.V2,
                  chainId: Byte = AddressScheme.current.chainId): CreateAliasTransaction = {
    CreateAliasTransaction.selfSigned(version, sender, name, fee, timestamp, chainId).explicitGet()
  }

  def ciFee(sc: Int = 0, nonNftIssue: Int = 0): Long =
    FeeUnit * FeeConstants(InvokeScriptTransaction.typeId) + (sc + 1) * ScriptExtraFee - 1 + nonNftIssue * FeeConstants(IssueTransaction.typeId) * FeeUnit

}

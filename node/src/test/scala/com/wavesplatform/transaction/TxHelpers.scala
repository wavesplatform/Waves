package com.wavesplatform.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.{DataEntry, StringDataEntry}
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction

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

  def genesis(address: Address, amount: Long = 100_000_000.waves): GenesisTransaction =
    GenesisTransaction.create(address, amount, timestamp).explicitGet()

  def transfer(
      from: KeyPair = defaultSigner,
      to: AddressOrAlias = secondAddress,
      amount: Long = 1.waves,
      asset: Asset = Waves,
      fee: Long = TestValues.fee,
      feeAsset: Asset = Waves,
      version: Byte = TxVersion.V2
  ): TransferTransaction =
    TransferTransaction.selfSigned(version, from, to, asset, amount, feeAsset, fee, ByteStr.empty, timestamp).explicitGet()

  def issue(issuer: KeyPair = defaultSigner, amount: Long = 1000, script: Option[Script] = None): IssueTransaction =
    IssueTransaction
      .selfSigned(TxVersion.V2, issuer, "test", "", amount, 0, reissuable = true, script, 1.waves, timestamp)
      .explicitGet()

  def reissue(asset: IssuedAsset, amount: Long = 1000): ReissueTransaction =
    ReissueTransaction
      .selfSigned(TxVersion.V2, defaultSigner, asset, amount, reissuable = true, TestValues.fee, timestamp)
      .explicitGet()

  def dataEntry(account: KeyPair, value: DataEntry[_]): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V1, account, Seq(value), TestValues.fee * 3, timestamp).explicitGet()

  def data(account: KeyPair = defaultSigner, key: String = "test", value: String = "test"): DataTransaction =
    dataWithMultipleEntries(account, Seq(StringDataEntry(key, value)))

  def dataWithMultipleEntries(account: KeyPair, entries: Seq[DataEntry[_]]): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V1, account, entries, TestValues.fee * 3, timestamp).explicitGet()

  def orderV3(orderType: OrderType, asset: Asset, feeAsset: Asset): Order = {
    orderV3(orderType, asset, Waves, feeAsset)
  }

  def order(orderType: OrderType, asset: Asset): Order =
    orderV3(orderType, asset, Waves)

  def orderV3(
      orderType: OrderType,
      amountAsset: Asset,
      priceAsset: Asset,
      feeAsset: Asset,
      amount: TxAmount = 1L,
      price: TxAmount = 1L,
      fee: TxAmount = 1L,
      sender: KeyPair = defaultSigner,
      matcher: KeyPair = defaultSigner
  ): Order = {
    Order.selfSigned(
      TxVersion.V3,
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

  def exchange(order1: Order, order2: Order, matcher: KeyPair = defaultSigner): ExchangeTransaction = {
    ExchangeTransaction
      .signed(
        TxVersion.V2,
        matcher.privateKey,
        order1,
        order2,
        order1.amount,
        order1.price,
        order1.matcherFee,
        order2.matcherFee,
        TestValues.fee,
        timestamp
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

  def setScript(acc: KeyPair, script: Script): SetScriptTransaction = {
    SetScriptTransaction.selfSigned(TxVersion.V1, acc, Some(script), TestValues.fee, timestamp).explicitGet()
  }

  def invoke(
      dApp: AddressOrAlias,
      func: Option[String] = Some("test"),
      args: Seq[EXPR] = Nil,
      payments: Seq[Payment] = Nil,
      invoker: KeyPair = defaultSigner,
      fee: Long = TestValues.fee,
      feeAssetId: Asset = Waves
  ): InvokeScriptTransaction = {
    val fc = func.map(name => functionCall(name, args: _*))
    InvokeScriptTransaction.selfSigned(TxVersion.V1, invoker, dApp, fc, payments, fee, feeAssetId, timestamp).explicitGet()
  }

  def functionCall(func: String, args: EXPR*): FUNCTION_CALL = {
    FUNCTION_CALL(FunctionHeader.User(func), args.toList)
  }

  def lease(recipient: AddressOrAlias = secondAddress, amount: TxAmount = 10.waves): LeaseTransaction = {
    LeaseTransaction.selfSigned(TxVersion.V2, defaultSigner, recipient, amount, TestValues.fee, timestamp).explicitGet()
  }

  def leaseCancel(leaseId: ByteStr): LeaseCancelTransaction = {
    LeaseCancelTransaction.selfSigned(TxVersion.V2, defaultSigner, leaseId, TestValues.fee, timestamp).explicitGet()
  }
}

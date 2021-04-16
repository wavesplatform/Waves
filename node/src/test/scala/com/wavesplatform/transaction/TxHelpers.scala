package com.wavesplatform.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.assets.{IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction

object TxHelpers {
  def signer(i: Int): KeyPair  = KeyPair(Ints.toByteArray(i))
  def address(i: Int): Address = signer(i).toAddress

  val defaultSigner: KeyPair  = signer(0)
  val defaultAddress: Address = defaultSigner.toAddress
  val secondSigner: KeyPair   = signer(1)
  val secondAddress: Address  = secondSigner.toAddress

  private[this] var lastTimestamp = System.currentTimeMillis()
  def timestamp: Long = {
    lastTimestamp += 1
    lastTimestamp
  }

  def genesis(address: Address, amount: Long = 1000.waves): GenesisTransaction =
    GenesisTransaction.create(address, amount, timestamp).explicitGet()

  def transfer(from: KeyPair = defaultSigner, to: AddressOrAlias = secondAddress, amount: Long = 1.waves, asset: Asset = Waves): TransferTransaction =
    TransferTransaction.selfSigned(TxVersion.V1, from, to, asset, amount, Waves, TestValues.fee, ByteStr.empty, timestamp).explicitGet()

  def issue(amount: Long = 1000, script: Script = null): IssueTransaction =
    IssueTransaction
      .selfSigned(TxVersion.V2, defaultSigner, "test", "", amount, 0, reissuable = true, Option(script), 1.waves, timestamp)
      .explicitGet()

  def reissue(asset: IssuedAsset, amount: Long = 1000): ReissueTransaction =
    ReissueTransaction
      .selfSigned(TxVersion.V2, defaultSigner, asset, amount, reissuable = true, TestValues.fee, timestamp)
      .explicitGet()

  def data(account: KeyPair = defaultSigner, key: String = "test", value: String = "test"): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V1, account, Seq(StringDataEntry(key, value)), TestValues.fee * 3, timestamp).explicitGet()

  def orderV3(orderType: OrderType, asset: Asset, feeAsset: Asset): Order = {
    orderV3(orderType, asset, Waves, feeAsset)
  }

  def order(orderType: OrderType, asset: Asset): Order =
    orderV3(orderType, asset, Waves)

  def orderV3(orderType: OrderType, amountAsset: Asset, priceAsset: Asset, feeAsset: Asset): Order = {
    Order.selfSigned(
      TxVersion.V3,
      defaultSigner,
      defaultSigner.publicKey,
      AssetPair(amountAsset, priceAsset),
      orderType,
      1L,
      1L,
      timestamp,
      timestamp + 100000,
      1L,
      feeAsset
    )
  }

  def exchange(order1: Order, order2: Order): ExchangeTransaction = {
    ExchangeTransaction
      .signed(
        TxVersion.V2,
        defaultSigner.privateKey,
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

  def setScript(acc: KeyPair, script: Script): SetScriptTransaction = {
    SetScriptTransaction.selfSigned(TxVersion.V1, acc, Some(script), TestValues.fee, timestamp).explicitGet()
  }

  def invoke(
      dApp: AddressOrAlias,
      func: String,
      args: Seq[EXPR] = Nil,
      payments: Seq[Payment] = Nil,
      fee: Long = TestValues.fee,
      feeAssetId: Asset = Waves
  ): InvokeScriptTransaction = {
    val fc = FUNCTION_CALL(FunctionHeader.User(func), args.toList)
    InvokeScriptTransaction.selfSigned(TxVersion.V1, defaultSigner, dApp, Some(fc), payments, fee, feeAssetId, timestamp).explicitGet()
  }

  def lease(recipient: AddressOrAlias = secondAddress, amount: TxAmount = 10.waves): LeaseTransaction = {
    LeaseTransaction.selfSigned(TxVersion.V2, defaultSigner, recipient, amount, TestValues.fee, timestamp).explicitGet()
  }

  def leaseCancel(leaseId: ByteStr): LeaseCancelTransaction = {
    LeaseCancelTransaction.selfSigned(TxVersion.V2, defaultSigner, leaseId, TestValues.fee, timestamp).explicitGet()
  }
}

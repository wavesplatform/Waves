package com.wavesplatform.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.transfer.TransferTransaction

object TxHelpers {
  def signer(i: Int): KeyPair = KeyPair(Ints.toByteArray(i))
  val defaultSigner: KeyPair  = signer(0)

  private[this] var lastTimestamp = System.currentTimeMillis()
  def timestamp: Long = {
    lastTimestamp += 1
    lastTimestamp
  }

  def genesis(address: Address, amount: Long = TestValues.OneWaves * 1000): GenesisTransaction =
    GenesisTransaction.create(address, amount, timestamp).explicitGet()

  def transfer(from: KeyPair, to: AddressOrAlias, amount: Long = TestValues.OneWaves, asset: Asset = Waves): TransferTransaction =
    TransferTransaction.selfSigned(TxVersion.V1, from, to, asset, amount, Waves, TestValues.fee, ByteStr.empty, timestamp).explicitGet()

  def issue(amount: Long = TestValues.ThousandWaves, script: Script = null): IssueTransaction =
    IssueTransaction
      .selfSigned(TxVersion.V2, defaultSigner, "test", "", amount, 0, reissuable = false, Option(script), TestValues.OneWaves, timestamp)
      .explicitGet()

  def orderV3(orderType: OrderType, asset: Asset, feeAsset: Asset): Order = {
    Order.selfSigned(
      TxVersion.V3,
      defaultSigner,
      defaultSigner.publicKey,
      AssetPair(asset, Waves),
      orderType,
      1,
      1,
      timestamp,
      timestamp + 100000,
      1,
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
}

package com.wavesplatform.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.TestValues
import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.{DataEntry, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.utils.Signed
import org.web3j.crypto.ECKeyPair

object TxHelpers {
  def signer(i: Int): KeyPair  = KeyPair(Ints.toByteArray(i))
  def address(i: Int): Address = signer(i).toAddress

  def defaultSigner: KeyPair  = signer(0)
  def defaultAddress: Address = defaultSigner.toAddress
  def secondSigner: KeyPair   = signer(1)
  def secondAddress: Address  = secondSigner.toAddress
  def voidAddress: Address    = Address(new Array[Byte](20))

  def defaultEthSigner: ECKeyPair = defaultSigner.toEthKeyPair

  val matcher: KeyPair = defaultSigner

  private[this] var lastTimestamp = System.currentTimeMillis()
  def timestamp: Long = {
    lastTimestamp += 1
    lastTimestamp
  }

  @throws[IllegalArgumentException]
  def signature(sig: String): Proofs =
    Proofs(ByteStr.decodeBase58(sig).get)

  def genesis(address: Address, amount: Long = 100000000.waves): GenesisTransaction =
    GenesisTransaction.create(address, amount, timestamp).explicitGet()

  def transfer(
      from: KeyPair = defaultSigner,
      to: AddressOrAlias = secondAddress,
      amount: Long = 1.waves,
      asset: Asset = Waves,
      fee: Long = TestValues.fee,
      version: Byte = TxVersion.V2
  ): TransferTransaction =
    TransferTransaction.selfSigned(version, from, to, asset, amount, Waves, fee, ByteStr.empty, timestamp).explicitGet()

  def issue(amount: Long = 1000, script: Script = null): IssueTransaction =
    IssueTransaction
      .selfSigned(TxVersion.V2, defaultSigner, "test", "", amount, 0, reissuable = true, Option(script), 1.waves, timestamp)
      .explicitGet()

  def reissue(asset: IssuedAsset, amount: Long = 1000): ReissueTransaction =
    ReissueTransaction
      .selfSigned(TxVersion.V2, defaultSigner, asset, amount, reissuable = true, TestValues.fee, timestamp)
      .explicitGet()

  def dataEntry(account: KeyPair, value: DataEntry[?]): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V1, account, Seq(value), TestValues.fee * 3, timestamp).explicitGet()

  def data(account: KeyPair = defaultSigner, key: String = "test", value: String = "test"): DataTransaction =
    dataWithMultipleEntries(account, Seq(StringDataEntry(key, value)))

  def dataWithMultipleEntries(account: KeyPair, entries: Seq[DataEntry[?]]): DataTransaction =
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

  def exchange(
      order1: Order,
      order2: Order,
      version: TxVersion = TxVersion.V2,
      timestamp: TxTimestamp = this.timestamp,
      matcher: KeyPair = defaultSigner
  ): ExchangeTransaction = {
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
        timestamp
      )
      .explicitGet()
  }

  def script(scriptText: String): Script = {
    val (script, _) = ScriptCompiler.compile(scriptText, ScriptEstimatorV3(fixOverflow = true, overhead = true)).explicitGet()
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

  def freeCallScript(scriptText: String, version: StdLibVersion = StdLibVersion.V6): ExprScriptImpl =
    TestCompiler(version).compileFreeCall(scriptText) match {
      case es: ExprScriptImpl => es
      case other              => throw new IllegalStateException(s"Not an expression: $other")
    }

  def scriptV5(scriptText: String): ContractScriptImpl =
    script(s"""
              |{-# STDLIB_VERSION 5 #-}
              |{-# CONTENT_TYPE DAPP #-}
              |
              |$scriptText
              |""".stripMargin) match {
      case cs: ContractScriptImpl => cs
      case other                  => throw new IllegalStateException(s"Not a contract: $other")
    }

  def scriptV6(scriptText: String): ContractScriptImpl =
    script(s"""
              |{-# STDLIB_VERSION 6 #-}
              |{-# CONTENT_TYPE DAPP #-}
              |
              |$scriptText
              |""".stripMargin) match {
      case cs: ContractScriptImpl => cs
      case other                  => throw new IllegalStateException(s"Not a contract: $other")
    }

  def estimate(script: Script): Int =
    math.toIntExact(
      Script
        .estimate(
          script,
          ScriptEstimatorV3(fixOverflow = true, overhead = false),
          fixEstimateOfVerifier = true,
          useContractVerifierLimit = false
        )
        .explicitGet()
    )

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
    val fc = functionCall(func, args*)
    Signed.invokeScript(TxVersion.V1, defaultSigner, dApp, Some(fc), payments, fee, feeAssetId, timestamp)
  }

  def invokeExpression(
      expression: ExprScript,
      fee: Long = TestValues.fee,
      feeAssetId: Asset = Waves
  ): InvokeExpressionTransaction =
    InvokeExpressionTransaction.selfSigned(TxVersion.V1, defaultSigner, expression, fee, feeAssetId, timestamp).explicitGet()

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

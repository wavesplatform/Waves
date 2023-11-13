package com.wavesplatform.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.TestValues
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit, ScriptExtraFee}
import com.wavesplatform.state.{DataEntry, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.utils.Signed
import monix.execution.atomic.AtomicLong
import org.web3j.crypto.ECKeyPair

object TxHelpers {
  def signer(i: Int): SeedKeyPair = KeyPair(Ints.toByteArray(i))
  def address(i: Int): Address    = signer(i).toAddress

  val defaultSigner: SeedKeyPair = signer(0)
  val defaultAddress: Address    = defaultSigner.toAddress
  val secondSigner: SeedKeyPair  = signer(1)
  val secondAddress: Address     = secondSigner.toAddress

  val defaultEthSigner: ECKeyPair = defaultSigner.toEthKeyPair

  def accountSeqGenerator(numberAccounts: Int, amount: Long): Seq[ParsedTransfer] = {
    val firstAccountNum = 100
    val lastAccountNum  = firstAccountNum + numberAccounts
    val accountsSeq = (firstAccountNum until lastAccountNum).map { num =>
      val recipient = signer(num).toAddress
      ParsedTransfer(recipient, TxNonNegativeAmount.unsafeFrom(amount))
    }
    accountsSeq
  }

  val matcher: SeedKeyPair = defaultSigner

  private[this] val lastTimestamp = AtomicLong(System.currentTimeMillis())
  def timestamp: Long             = lastTimestamp.getAndIncrement()

  @throws[IllegalArgumentException]
  def signature(sig: String): Proofs =
    Proofs(ByteStr.decodeBase58(sig).get)

  def genesis(address: Address, amount: Long = ENOUGH_AMT, timestamp: TxTimestamp = timestamp): GenesisTransaction =
    GenesisTransaction.create(address, amount, timestamp).explicitGet()

  def payment(from: KeyPair = defaultSigner, to: Address = secondAddress, amount: Long = 1.waves): PaymentTransaction =
    PaymentTransaction.create(from, to, amount, TestValues.fee, timestamp).explicitGet()

  def transfer(
      from: KeyPair = defaultSigner,
      to: AddressOrAlias = secondAddress,
      amount: Long = 1.waves,
      asset: Asset = Waves,
      fee: Long = TestValues.fee,
      feeAsset: Asset = Waves,
      attachment: ByteStr = ByteStr.empty,
      timestamp: TxTimestamp = timestamp,
      version: Byte = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): TransferTransaction =
    TransferTransaction.selfSigned(version, from, to, asset, amount, feeAsset, fee, attachment, timestamp, chainId).explicitGet()

  def transferUnsigned(
      from: KeyPair = defaultSigner,
      to: AddressOrAlias = secondAddress,
      amount: Long = 1.waves,
      asset: Asset = Waves,
      fee: Long = TestValues.fee,
      feeAsset: Asset = Waves,
      version: Byte = TxVersion.V2
  ): TransferTransaction =
    TransferTransaction(
      version,
      from.publicKey,
      to,
      asset,
      TxPositiveAmount.unsafeFrom(amount),
      feeAsset,
      TxPositiveAmount.unsafeFrom(fee),
      ByteStr.empty,
      timestamp,
      Proofs.empty,
      to.chainId
    )

  def massTransfer(
      from: KeyPair = defaultSigner,
      to: Seq[ParsedTransfer] = Seq(ParsedTransfer(secondAddress, TxNonNegativeAmount.unsafeFrom(1.waves))),
      asset: Asset = Waves,
      fee: Long = FeeConstants(TransactionType.MassTransfer) * FeeUnit,
      timestamp: TxTimestamp = timestamp,
      version: Byte = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): MassTransferTransaction =
    MassTransferTransaction.selfSigned(version, from, asset, to, fee, timestamp, ByteStr.empty, chainId).explicitGet()

  def issue(
      issuer: KeyPair = defaultSigner,
      amount: Long = Long.MaxValue / 100,
      decimals: Byte = 0,
      name: String = "test",
      description: String = "description",
      fee: Long = 1.waves,
      script: Option[Script] = None,
      reissuable: Boolean = true,
      timestamp: TxTimestamp = timestamp,
      version: TxVersion = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): IssueTransaction =
    IssueTransaction
      .selfSigned(version, issuer, name, description, amount, decimals, reissuable, script, fee, timestamp, chainId)
      .explicitGet()

  def reissue(
      asset: IssuedAsset,
      sender: KeyPair = defaultSigner,
      amount: Long = 1000,
      reissuable: Boolean = true,
      fee: Long = TestValues.fee,
      version: TxVersion = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): ReissueTransaction =
    ReissueTransaction
      .selfSigned(version, sender, asset, amount, reissuable = reissuable, fee, timestamp, chainId)
      .explicitGet()

  def dataEntry(account: KeyPair, value: DataEntry[?]): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V1, account, Seq(value), TestValues.fee * 3, timestamp).explicitGet()

  def dataSingle(account: KeyPair = defaultSigner, key: String = "test", value: String = "test", fee: Long = TestValues.fee): DataTransaction =
    data(account, Seq(StringDataEntry(key, value)), fee)

  def data(account: KeyPair, entries: Seq[DataEntry[?]], fee: Long = TestValues.fee * 3, version: TxVersion = TxVersion.V1): DataTransaction =
    DataTransaction.selfSigned(version, account, entries, fee, timestamp).explicitGet()

  def dataV2(
      account: KeyPair,
      entries: Seq[DataEntry[?]],
      fee: Long = TestValues.fee * 3,
      chainId: Byte = AddressScheme.current.chainId
  ): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V2, account, entries, fee, timestamp, chainId).explicitGet()

  def dataWithMultipleEntries(account: KeyPair, entries: Seq[DataEntry[?]]): DataTransaction =
    DataTransaction.selfSigned(TxVersion.V1, account, entries, TestValues.fee * 3, timestamp).explicitGet()

  def burn(
      asset: IssuedAsset,
      amount: Long = 1,
      sender: KeyPair = defaultSigner,
      fee: Long = TestValues.fee,
      version: TxVersion = TxVersion.V3,
      chainId: Byte = AddressScheme.current.chainId
  ): BurnTransaction =
    BurnTransaction.selfSigned(version, sender, asset, amount, fee, timestamp, chainId).explicitGet()

  def updateAssetInfo(
      assetId: ByteStr,
      name: String = "updated_name",
      desc: String = "updated_desc",
      sender: KeyPair = defaultSigner,
      fee: Long = TestValues.fee,
      feeAsset: Asset = Waves,
      version: TxVersion = TxVersion.V1,
      chainId: Byte = AddressScheme.current.chainId
  ): UpdateAssetInfoTransaction =
    UpdateAssetInfoTransaction.selfSigned(version, sender, assetId, name, desc, timestamp, fee, feeAsset, chainId).explicitGet()

  def orderV3(orderType: OrderType, asset: Asset, feeAsset: Asset = Waves): Order = {
    order(orderType, asset, Waves, feeAsset)
  }

  def order(
      orderType: OrderType,
      amountAsset: Asset,
      priceAsset: Asset,
      feeAsset: Asset = Waves,
      amount: Long = 1L,
      price: Long = 1L,
      priceMode: OrderPriceMode = OrderPriceMode.Default,
      fee: Long = 1L,
      sender: KeyPair = defaultSigner,
      matcher: KeyPair = defaultSigner,
      timestamp: TxTimestamp = timestamp,
      expiration: TxTimestamp = timestamp + 100000,
      version: TxVersion = Order.V3,
      attachment: Option[ByteStr] = None
  ): Order = {
    Order
      .selfSigned(
        version,
        sender,
        matcher.publicKey,
        AssetPair(amountAsset, priceAsset),
        orderType,
        amount,
        price,
        timestamp,
        expiration,
        fee,
        feeAsset,
        priceMode,
        attachment
      )
      .explicitGet()
  }

  def exchangeFromOrders(
      order1: Order,
      order2: Order,
      matcher: KeyPair = defaultSigner,
      fee: Long = TestValues.fee,
      version: TxVersion = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): ExchangeTransaction = exchangeFromOrders(order1, order2, order1.price.value, matcher, fee, version, chainId)

  def exchangeFromOrders(
      order1: Order,
      order2: Order,
      price: Long,
      matcher: KeyPair,
      fee: Long,
      version: TxVersion,
      chainId: Byte
  ): ExchangeTransaction =
    ExchangeTransaction
      .signed(
        version,
        matcher.privateKey,
        order1,
        order2,
        order1.amount.value,
        price,
        order1.matcherFee.value,
        order2.matcherFee.value,
        fee,
        timestamp,
        chainId
      )
      .explicitGet()

  def exchange(
      order1: Order,
      order2: Order,
      matcher: KeyPair = defaultSigner,
      amount: Long = 1L,
      price: Long = 1L,
      buyMatcherFee: Long = 1L,
      sellMatcherFee: Long = 1L,
      fee: Long = TestValues.fee,
      timestamp: Long = timestamp,
      version: TxVersion = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): ExchangeTransaction =
    ExchangeTransaction
      .signed(
        version,
        matcher = matcher.privateKey,
        order1 = order1,
        order2 = order2,
        amount = amount,
        price = price,
        buyMatcherFee = buyMatcherFee,
        sellMatcherFee = sellMatcherFee,
        fee = fee,
        timestamp = timestamp,
        chainId = chainId
      )
      .explicitGet()

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

  def freeCallScript(scriptText: String, version: StdLibVersion = V6): ExprScriptImpl =
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

  def setScript(
      acc: KeyPair,
      script: Script,
      fee: Long = FeeConstants(TransactionType.SetScript) * FeeUnit,
      version: TxVersion = TxVersion.V1,
      chainId: Byte = AddressScheme.current.chainId,
      timestamp: TxTimestamp = timestamp
  ): SetScriptTransaction = {
    SetScriptTransaction.selfSigned(version, acc, Some(script), fee, timestamp, chainId).explicitGet()
  }

  def setAssetScript(
      acc: KeyPair,
      asset: IssuedAsset,
      script: Script,
      fee: Long = FeeConstants(TransactionType.SetAssetScript) * FeeUnit + ScriptExtraFee,
      timestamp: TxTimestamp = timestamp,
      version: TxVersion = TxVersion.V1,
      chainId: Byte = AddressScheme.current.chainId
  ): SetAssetScriptTransaction = {
    SetAssetScriptTransaction.selfSigned(version, acc, asset, Some(script), fee, timestamp, chainId).explicitGet()
  }

  def invoke(
      dApp: AddressOrAlias = secondAddress,
      func: Option[String] = None,
      args: Seq[EXPR] = Nil,
      payments: Seq[Payment] = Nil,
      invoker: KeyPair = defaultSigner,
      fee: Long = FeeConstants(TransactionType.InvokeScript) * FeeUnit,
      feeAssetId: Asset = Waves,
      version: TxVersion = TxVersion.V2,
      timestamp: TxTimestamp = timestamp
  ): InvokeScriptTransaction = {
    val fc = func.map(name => functionCall(name, args*))
    Signed.invokeScript(version, invoker, dApp, fc, payments, fee, feeAssetId, timestamp)
  }

  def invokeExpression(
      expression: ExprScript,
      sender: KeyPair = defaultSigner,
      fee: Long = TestValues.fee,
      feeAssetId: Asset = Waves
  ): InvokeExpressionTransaction =
    InvokeExpressionTransaction.selfSigned(TxVersion.V1, sender, expression, fee, feeAssetId, timestamp).explicitGet()

  def functionCall(func: String, args: EXPR*): FUNCTION_CALL = {
    FUNCTION_CALL(FunctionHeader.User(func), args.toList)
  }

  def lease(
      sender: KeyPair = defaultSigner,
      recipient: AddressOrAlias = secondAddress,
      amount: Long = 10.waves,
      fee: Long = FeeConstants(TransactionType.Lease) * FeeUnit,
      timestamp: TxTimestamp = timestamp,
      version: TxVersion = TxVersion.V2
  ): LeaseTransaction = {
    LeaseTransaction.selfSigned(version, sender, recipient, amount, fee, timestamp).explicitGet()
  }

  def leaseCancel(
      leaseId: ByteStr,
      sender: KeyPair = defaultSigner,
      fee: Long = FeeConstants(TransactionType.LeaseCancel) * FeeUnit,
      timestamp: TxTimestamp = timestamp,
      version: TxVersion = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): LeaseCancelTransaction = {
    LeaseCancelTransaction.selfSigned(version, sender, leaseId, fee, timestamp, chainId).explicitGet()
  }

  def sponsor(
      asset: IssuedAsset,
      minSponsoredAssetFee: Option[Long] = Some(TestValues.fee),
      sender: KeyPair = defaultSigner,
      fee: Long = 1.waves,
      version: TxVersion = TxVersion.V1,
      chainId: Byte = AddressScheme.current.chainId
  ): SponsorFeeTransaction = {
    SponsorFeeTransaction.selfSigned(version, sender, asset, minSponsoredAssetFee, fee, timestamp, chainId).explicitGet()
  }

  def createAlias(
      name: String,
      sender: KeyPair = defaultSigner,
      fee: Long = TestValues.fee,
      version: TxVersion = TxVersion.V2,
      chainId: Byte = AddressScheme.current.chainId
  ): CreateAliasTransaction = {
    CreateAliasTransaction.selfSigned(version, sender, name, fee, timestamp, chainId).explicitGet()
  }

  def ciFee(sc: Int = 0, nonNftIssue: Int = 0, freeCall: Boolean = false): Long =
    invokeFee(freeCall) + (sc + 1) * ScriptExtraFee - 1 + nonNftIssue * FeeConstants(TransactionType.Issue) * FeeUnit

  private def invokeFee(freeCall: Boolean) =
    if (freeCall)
      FeeUnit * FeeConstants(TransactionType.InvokeExpression)
    else
      FeeUnit * FeeConstants(TransactionType.InvokeScript)
}

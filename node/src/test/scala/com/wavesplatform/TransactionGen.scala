package com.wavesplatform

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.testing.{ScriptGen, TypedScriptGen}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.settings.Constants
import com.wavesplatform.state.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.lease.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.utils.Signed
import org.scalacheck.Gen.{alphaLowerChar, alphaUpperChar, frequency, numChar}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite
import org.web3j.crypto.ECKeyPair

import scala.concurrent.duration.*
import scala.util.Random

trait TransactionGenBase extends ScriptGen with TypedScriptGen with NTPTime { _: Suite =>

  val ScriptExtraFee                  = 400000L
  protected def waves(n: Float): Long = (n * 100000000L).toLong

  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])

  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val bytes64gen: Gen[Array[Byte]] = byteArrayGen(64)
  val attachmentGen: Gen[ByteStr]  = bytes32gen.map(ByteStr(_))

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    for {
      length <- Gen.chooseNum(minSize, maxSize)
      bytes  <- byteArrayGen(length)
    } yield bytes

  def genBoundedString(minSize: Int, maxSize: Int): Gen[String] = {
    genBoundedStringBytes(minSize, maxSize).map(new String(_))
  }

  def genBoundedStringBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz =>
      Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray)
    }
  }

  val accountGen: Gen[KeyPair] = bytes32gen.map(seed => KeyPair(seed))

  val ethAccountGen: Gen[ECKeyPair] = accountGen.map(_.toEthKeyPair)

  val aliasSymbolChar: Gen[Char] = Gen.oneOf('.', '@', '_', '-')

  val invalidAliasSymbolChar: Gen[Char] = Gen.oneOf('~', '`', '!', '#', '$', '%', '^', '&', '*', '=', '+')

  val aliasAlphabetGen: Gen[Char] = frequency((1, numChar), (1, aliasSymbolChar), (9, alphaLowerChar))

  val invalidAliasAlphabetGen: Gen[Char] = frequency((1, numChar), (3, invalidAliasSymbolChar), (9, alphaUpperChar))

  val validAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, aliasAlphabetGen)
  } yield aliasChars.mkString

  val aliasGen: Gen[Alias] = for {
    str <- validAliasStringGen
  } yield Alias.create(str.mkString).explicitGet()

  val invalidAliasStringGen: Gen[String] = for {
    length     <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, invalidAliasAlphabetGen)
  } yield aliasChars.mkString

  val accountOrAliasGen: Gen[AddressOrAlias] = Gen.oneOf(aliasGen, accountGen.map(_.toAddress))

  val positiveLongGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L / 100)
  val positiveIntGen: Gen[Int]   = Gen.choose(1, Int.MaxValue / 100)
  val smallFeeGen: Gen[Long]     = Gen.choose(400000L, 100000000L)

  val maxOrderTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + ntpTime.correctedTime())
  val timestampGen: Gen[Long]    = Gen.choose(1L, Long.MaxValue - 100)
  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(ntpTime.correctedTime() - _)

  def validTimestampGen(blockTimestamp: Long, back: FiniteDuration = 120.minutes, forward: FiniteDuration = 90.minutes): Gen[Long] =
    Gen.choose(blockTimestamp - back.toMillis, blockTimestamp + forward.toMillis)

  val wavesAssetGen: Gen[Option[ByteStr]] = Gen.const(None)
  val assetIdGen: Gen[Option[ByteStr]]    = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen.map(ByteStr(_)))))

  val assetPairGen: Gen[AssetPair] = assetIdGen.flatMap {
    case None => bytes32gen.map(b => AssetPair(Waves, IssuedAsset(ByteStr(b))))
    case Some(a1bytes) =>
      val a2bytesGen: Gen[Option[Array[Byte]]] = byteArrayGen(31).map(a2bytes => Option(a1bytes.arr(0) +: a2bytes))

      Gen.oneOf(Gen.const(None), a2bytesGen).map { a2 =>
        val asset1 = IssuedAsset(a1bytes)
        val asset2 = a2.fold[Asset](Waves)(arr => IssuedAsset(ByteStr(arr)))
        AssetPair(asset1, asset2)
      }
  }

  val MinIssueFee = 100000000L

  val issueParamGen: Gen[(KeyPair, Array[Byte], Array[Byte], Long, Byte, Boolean, Long, TxTimestamp)] = for {
    sender      <- accountGen
    assetName   <- genBoundedStringBytes(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedStringBytes(0, IssueTransaction.MaxAssetDescriptionLength)
    quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
    decimals    <- Gen.choose(0: Byte, 8: Byte)
    reissuable  <- Arbitrary.arbitrary[Boolean]
    fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp = System.currentTimeMillis()
  } yield (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp)

  val proofsGen: Gen[Proofs] = for {
    proofsAmount <- Gen.choose(1, 8)
    proofs       <- Gen.listOfN(proofsAmount, genBoundedBytes(0, 50))
  } yield Proofs.create(proofs.map(ByteStr(_))).explicitGet()

  val scriptGen: Gen[Script]         = exprGen.map(e => ExprScript(e).explicitGet())
  val contractScriptGen: Gen[Script] = contractGen.map(e => ContractScript(V3, e).explicitGet())
  val contractOrExpr: Gen[Script]    = Gen.oneOf(scriptGen, contractScriptGen)

  def issueAndSetAssetScriptGen(sender: KeyPair): Gen[(IssueTransaction, SetAssetScriptTransaction)] =
    for {
      (_, assetName, description, quantity, decimals, _, iFee, _) <- issueParamGen
      timestamp                                                   <- timestampGen
      script1                                                     <- scriptGen
      script2                                                     <- scriptGen
      issue = IssueTransaction
        .selfSigned(
          TxVersion.V2,
          sender,
          new String(assetName),
          new String(description),
          quantity,
          decimals,
          reissuable = true,
          Some(script1),
          iFee,
          timestamp
        )
        .explicitGet()
      setAssetScript = SetAssetScriptTransaction
        .selfSigned(1.toByte, sender, IssuedAsset(issue.id()), Some(script2), 1 * Constants.UnitsInWave + ScriptExtraFee, timestamp)
        .explicitGet()
    } yield (issue, setAssetScript)

  val setAssetScriptTransactionGen: Gen[(Seq[Transaction], SetAssetScriptTransaction)] =
    for {
      sender                  <- accountGen
      (issue, setAssetScript) <- issueAndSetAssetScriptGen(sender)
    } yield (Seq(issue), setAssetScript)

  val setScriptTransactionGen: Gen[SetScriptTransaction] = for {
    sender    <- accountGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
    script    <- Gen.option(contractOrExpr)
  } yield SetScriptTransaction.selfSigned(1.toByte, sender, script, fee, timestamp).explicitGet()

  private val leaseParamGen = for {
    sender    <- accountGen
    amount    <- positiveLongGen
    fee       <- smallFeeGen
    timestamp <- timestampGen
    recipient <- accountGen
  } yield (sender, amount, fee, timestamp, recipient)

  def createLease(sender: KeyPair, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias): Gen[LeaseTransaction] = {
    val v1 = LeaseTransaction.selfSigned(1.toByte, sender, recipient, amount, fee, timestamp).explicitGet()
    val v2 = LeaseTransaction.selfSigned(2.toByte, sender, recipient, amount, fee, timestamp).explicitGet()
    Gen.oneOf(v1, v2)
  }

  def createLeaseCancel(sender: KeyPair, leaseId: ByteStr, cancelFee: Long, timestamp: Long): Gen[LeaseCancelTransaction] = {
    val v1 = LeaseCancelTransaction.selfSigned(1.toByte, sender, leaseId, cancelFee, timestamp + 1).explicitGet()
    val v2 = LeaseCancelTransaction.selfSigned(2.toByte, sender, leaseId, cancelFee, timestamp + 1).explicitGet()
    Gen.oneOf(v1, v2)
  }
  val leaseAndCancelGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    lease                                       <- createLease(sender, amount, fee, timestamp, recipient.toAddress)
    cancelFee                                   <- smallFeeGen
    leaseCancel                                 <- createLeaseCancel(sender, lease.id(), cancelFee, timestamp + 1)
  } yield (lease, leaseCancel)

  def leaseAndCancelGeneratorP(leaseSender: KeyPair, recipient: AddressOrAlias, timestamp: Long): Gen[(LeaseTransaction, LeaseCancelTransaction)] =
    for {
      (_, amount, fee, _, _) <- leaseParamGen
      lease                  <- createLease(leaseSender, amount, fee, timestamp, recipient)
      fee2                   <- smallFeeGen
      unlease                <- createLeaseCancel(leaseSender, lease.id(), fee2, timestamp + 1)
    } yield (lease, unlease)

  val leaseGen: Gen[LeaseTransaction]             = leaseAndCancelGen.map(_._1)
  val leaseCancelGen: Gen[LeaseCancelTransaction] = leaseAndCancelGen.map(_._2)

  val transferParamGen: Gen[(Asset, KeyPair, AddressOrAlias, Long, TxTimestamp, Asset, TxTimestamp, ByteStr)] = for {
    amount     <- positiveLongGen
    feeAmount  <- smallFeeGen
    assetId    <- Gen.option(bytes32gen)
    feeAssetId <- Gen.option(bytes32gen)
    timestamp  <- timestampGen
    sender     <- accountGen
    attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient  <- accountOrAliasGen
  } yield (
    Asset.fromCompatId(assetId.map(ByteStr(_))),
    sender,
    recipient,
    amount,
    timestamp,
    Asset.fromCompatId(feeAssetId.map(ByteStr(_))),
    feeAmount,
    ByteStr(attachment)
  )

  def transferGeneratorP(sender: KeyPair, recipient: AddressOrAlias, assetId: Asset, feeAssetId: Asset): Gen[TransferTransaction] =
    for {
      (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(1.toByte, sender, recipient, assetId, amount, feeAssetId, feeAmount, attachment, timestamp).explicitGet()

  def versionedTransferGeneratorP(sender: KeyPair, recipient: AddressOrAlias, assetId: Asset, feeAssetId: Asset): Gen[TransferTransaction] =
    for {
      (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(2.toByte, sender, recipient, assetId, amount, feeAssetId, feeAmount, attachment, timestamp).explicitGet()

  def transferGeneratorP(timestamp: Long, sender: KeyPair, recipient: AddressOrAlias, maxAmount: Long): Gen[TransferTransaction] =
    for {
      amount                                    <- Gen.choose(1, maxAmount)
      (_, _, _, _, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(1.toByte, sender, recipient, Waves, amount, Waves, feeAmount, attachment, timestamp).explicitGet()

  def transferGeneratorP(timestamp: Long, sender: KeyPair, recipient: AddressOrAlias, assetId: Asset, feeAssetId: Asset): Gen[TransferTransaction] =
    for {
      (_, _, _, amount, _, _, feeAmount, attachment) <- transferParamGen
    } yield TransferTransaction.selfSigned(1.toByte, sender, recipient, assetId, amount, feeAssetId, feeAmount, attachment, timestamp).explicitGet()

  def wavesTransferGeneratorP(sender: KeyPair, recipient: AddressOrAlias): Gen[TransferTransaction] =
    transferGeneratorP(sender, recipient, Waves, Waves)

  def wavesTransferGeneratorP(timestamp: Long, sender: KeyPair, recipient: AddressOrAlias): Gen[TransferTransaction] =
    transferGeneratorP(timestamp, sender, recipient, Waves, Waves)

  def createWavesTransfer(
      sender: KeyPair,
      recipient: Address,
      amount: Long,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, TransferTransaction] =
    TransferTransaction.selfSigned(1.toByte, sender, recipient, Waves, amount, Waves, fee, ByteStr.empty, timestamp)

  val transferV1Gen: Gen[TransferTransaction] = (for {
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.selfSigned(1.toByte, sender, recipient, assetId, amount, feeAssetId, feeAmount, attachment, timestamp).explicitGet())
    .label("transferTransaction")

  val transferV2Gen: Gen[TransferTransaction] = (for {
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
    proofs                                                                             <- proofsGen
  } yield TransferTransaction
    .selfSigned(2.toByte, sender, recipient, assetId, amount, feeAssetId, feeAmount, attachment, timestamp)
    .explicitGet(
    ))
    .label("VersionedTransferTransaction")

  val createAliasGen: Gen[CreateAliasTransaction] =
    for {
      timestamp: Long <- positiveLongGen
      sender          <- accountGen
      alias: Alias    <- aliasGen
      tx <- Gen.oneOf(
        CreateAliasTransaction.selfSigned(Transaction.V1, sender, alias.name, MinIssueFee, timestamp).explicitGet(),
        CreateAliasTransaction.selfSigned(Transaction.V2, sender, alias.name, MinIssueFee, timestamp).explicitGet()
      )
    } yield tx

  def createAliasGen(sender: KeyPair, alias: Alias, fee: Long, timestamp: Long): Gen[CreateAliasTransaction] =
    for {
      tx <- Gen.oneOf(
        CreateAliasTransaction.selfSigned(Transaction.V1, sender, alias.name, fee, timestamp).explicitGet(),
        CreateAliasTransaction.selfSigned(Transaction.V2, sender, alias.name, fee, timestamp).explicitGet()
      )
    } yield tx

  val issueReissueBurnGen: Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    amount <- positiveLongGen
    sender <- accountGen
    r      <- issueReissueBurnGeneratorP(amount, amount, amount, sender)
  } yield r

  def createLegacyIssue(
      issuer: KeyPair,
      nameBytes: Array[Byte],
      descriptionBytes: Array[Byte],
      quantity: Long,
      decimals: Byte,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long
  ): Gen[IssueTransaction] =
    IssueTransaction
      .selfSigned(TxVersion.V1, issuer, new String(nameBytes), new String(descriptionBytes), quantity, decimals, reissuable, None, fee, timestamp)
      .explicitGet()

  def createReissue(
      reissuer: KeyPair,
      assetId: IssuedAsset,
      quantity: Long,
      reissuable: Boolean,
      fee: Long,
      timestamp: Long
  ): Gen[ReissueTransaction] = {
    for {
      tx <- Gen.oneOf(
        ReissueTransaction.selfSigned(1.toByte, reissuer, assetId, quantity, reissuable, fee, timestamp).explicitGet(),
        ReissueTransaction.selfSigned(2.toByte, reissuer, assetId, quantity, reissuable, fee, timestamp).explicitGet()
      )
    } yield tx
  }

  def createBurn(burner: KeyPair, assetId: IssuedAsset, amount: Long, fee: Long, timestamp: Long): Gen[BurnTransaction] = {
    for {
      tx <- Gen.oneOf(
        BurnTransaction.selfSigned(1.toByte, burner, assetId, amount, fee, timestamp).explicitGet(),
        BurnTransaction.selfSigned(2.toByte, burner, assetId, amount, fee, timestamp).explicitGet()
      )
    } yield tx
  }

  /** @param issueQuantity
    *   must be positive
    * @param reissueQuantity
    *   must be positive
    * @param burnQuantity
    *   must be positive
    */
  def issueReissueBurnGeneratorP(
      issueQuantity: Long,
      reissueQuantity: Long,
      burnQuantity: Long,
      sender: KeyPair
  ): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = {
    require(issueQuantity > 0 && reissueQuantity > 0 && burnQuantity > 0, "Asset amounts must be positive")
    for {
      (_, assetName, description, _, decimals, reissuable, iFee, timestamp) <- issueParamGen
      reissuable2                                                           <- Arbitrary.arbitrary[Boolean]
      fee                                                                   <- smallFeeGen
      issue   <- createLegacyIssue(sender, assetName, description, issueQuantity, decimals, reissuable, iFee, timestamp)
      reissue <- createReissue(sender, issue.asset, reissueQuantity, reissuable2, fee, timestamp)
      burn    <- createBurn(sender, issue.asset, burnQuantity, fee, timestamp)
    } yield (issue, reissue, burn)
  }

  val issueGen: Gen[IssueTransaction]     = issueReissueBurnGen.map(_._1)
  val reissueGen: Gen[ReissueTransaction] = issueReissueBurnGen.map(_._2)
  val burnGen: Gen[BurnTransaction]       = issueReissueBurnGen.map(_._3)

  def sponsorFeeCancelSponsorFeeGen(
      sender: KeyPair,
      reducedFee: Boolean = true
  ): Gen[(IssueTransaction, SponsorFeeTransaction, SponsorFeeTransaction, SponsorFeeTransaction)] =
    for {
      (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
      issue = IssueTransaction
        .selfSigned(
          TxVersion.V1,
          sender,
          new String(assetName),
          new String(description),
          quantity,
          decimals,
          reissuable = reissuable,
          script = None,
          iFee,
          timestamp
        )
        .explicitGet()
      minFee  <- smallFeeGen
      minFee1 <- smallFeeGen
      assetId = issue.asset
      fee     = (if (reducedFee) 0.001 * Constants.UnitsInWave else 1 * Constants.UnitsInWave.toDouble).toLong
    } yield (
      issue,
      SponsorFeeTransaction.selfSigned(1.toByte, sender, assetId, Some(minFee), fee, timestamp).explicitGet(),
      SponsorFeeTransaction.selfSigned(1.toByte, sender, assetId, Some(minFee1), fee, timestamp).explicitGet(),
      SponsorFeeTransaction.selfSigned(1.toByte, sender, assetId, None, fee, timestamp).explicitGet()
    )

  val sponsorFeeGen: Gen[SponsorFeeTransaction] = for {
    sender        <- accountGen
    (_, tx, _, _) <- sponsorFeeCancelSponsorFeeGen(sender)
  } yield {
    tx
  }

  val argGen: Gen[EXPR] = Gen.const(CONST_LONG(11))

  val funcCallGen: Gen[FUNCTION_CALL] = for {
    functionName <- genBoundedString(1, 32).map(_.filter(_.isLetter))
    amt          <- Gen.choose(0, ContractLimits.MaxInvokeScriptArgs)
    args         <- Gen.listOfN(amt, argGen)

  } yield FUNCTION_CALL(FunctionHeader.User(functionName), args)

  def invokeScriptGen(paymentsGen: Gen[Seq[Payment]]): Gen[InvokeScriptTransaction] =
    for {
      payments    <- paymentsGen
      sender      <- accountGen
      dappAddress <- accountGen
      fc          <- funcCallGen
      fee         <- smallFeeGen
      timestamp   <- timestampGen
    } yield Signed.invokeScript(1.toByte, sender, dappAddress.toAddress, Some(fc), payments, fee, Waves, timestamp)

  val paymentListGen: Gen[Seq[Payment]] =
    for {
      wavesPayment <- Gen.option(positiveLongGen.map(Payment(_, Waves)))
      assetPayment = for {
        asset <- bytes32gen.map(ByteStr(_)).map(IssuedAsset(_))
        amt   <- positiveLongGen
      } yield Payment(amt, asset)
      assetPayments <- Gen.listOfN[Payment](ContractLimits.MaxAttachedPaymentAmount - 1, assetPayment)
    } yield assetPayments ++ wavesPayment

  val priceGen: Gen[Long]            = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherAmountGen: Gen[Long]    = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherFeeAmountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)

  val orderTypeGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderParamGen: Gen[(KeyPair, KeyPair, AssetPair, OrderType, TxTimestamp, TxTimestamp, TxTimestamp, TxTimestamp, TxTimestamp)] = for {
    sender     <- accountGen
    matcher    <- accountGen
    pair       <- assetPairGen
    orderType  <- orderTypeGen
    amount     <- matcherAmountGen
    price      <- priceGen
    timestamp  <- timestampGen
    expiration <- maxOrderTimeGen
    matcherFee <- matcherFeeAmountGen
  } yield (sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee)

  val orderV1Gen: Gen[Order] = for {
    (sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee) <- orderParamGen
  } yield Order.selfSigned(1.toByte, sender, matcher.publicKey, pair, orderType, price, amount, timestamp, expiration, matcherFee).explicitGet()

  val orderV2Gen: Gen[Order] = for {
    (sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee) <- orderParamGen
  } yield Order.selfSigned(2.toByte, sender, matcher.publicKey, pair, orderType, amount, price, timestamp, expiration, matcherFee).explicitGet()

  val orderV3Gen: Gen[Order] = for {
    (sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee) <- orderParamGen
    matcherFeeAssetId                                                                    <- assetIdGen
  } yield Order
    .selfSigned(
      3.toByte,
      sender,
      matcher.publicKey,
      pair,
      orderType,
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      Asset.fromCompatId(matcherFeeAssetId)
    )
    .explicitGet()

  val orderGen: Gen[Order] = Gen.oneOf(orderV1Gen, orderV2Gen, orderV3Gen)

  val arbitraryOrderGen: Gen[Order] = for {
    (sender, matcher, pair, orderType, _, _, _, _, _) <- orderParamGen
    amount                                            <- Arbitrary.arbitrary[Long]
    price                                             <- Arbitrary.arbitrary[Long]
    timestamp                                         <- Arbitrary.arbitrary[Long]
    expiration                                        <- Arbitrary.arbitrary[Long]
    matcherFee                                        <- Arbitrary.arbitrary[Long]
  } yield Order.selfSigned(1: Byte, sender, matcher.publicKey, pair, orderType, amount, price, timestamp, expiration, matcherFee).explicitGet()

  val exchangeTransactionGen: Gen[ExchangeTransaction] = for {
    sender1                 <- accountGen
    sender2                 <- accountGen
    assetPair               <- assetPairGen
    buyerAnotherAsset       <- assetIdGen.map(Asset.fromCompatId)
    sellerAnotherAsset      <- assetIdGen.map(Asset.fromCompatId)
    buyerMatcherFeeAssetId  <- Gen.oneOf(assetPair.amountAsset, assetPair.priceAsset, buyerAnotherAsset, Waves)
    sellerMatcherFeeAssetId <- Gen.oneOf(assetPair.amountAsset, assetPair.priceAsset, sellerAnotherAsset, Waves)
    r <- Gen.oneOf(
      exchangeV1GeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset),
      exchangeV2GeneratorP(
        buyer = sender1,
        seller = sender2,
        amountAssetId = assetPair.amountAsset,
        priceAssetId = assetPair.priceAsset,
        buyMatcherFeeAssetId = buyerMatcherFeeAssetId,
        sellMatcherFeeAssetId = sellerMatcherFeeAssetId
      )
    )
  } yield r

  def exchangeV1GeneratorP(
      buyer: KeyPair,
      seller: KeyPair,
      amountAssetId: Asset,
      priceAssetId: Asset,
      fixedMatcherFee: Option[Long] = None,
      chainId: Byte = AddressScheme.current.chainId,
      fixedMatcher: Option[KeyPair] = None
  ): Gen[ExchangeTransaction] =
    for {
      (_, genMatcher, _, _, amount1, price, timestamp, expiration, genMatcherFee) <- orderParamGen
      amount2: Long                                                               <- matcherAmountGen
      matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
      assetPair = AssetPair(amountAssetId, priceAssetId)
    } yield {
      val matcherFee = fixedMatcherFee.getOrElse(genMatcherFee)
      val matcher    = fixedMatcher.getOrElse(genMatcher)
      val o1 =
        Order
          .buy(1: Byte, buyer, matcher.publicKey, assetPair, amount1, price, timestamp, expiration, matcherFee, priceMode = OrderPriceMode.Default)
          .explicitGet()
      val o2 = Order
        .sell(
          1: Byte,
          seller,
          matcher.publicKey,
          assetPair,
          amount2,
          price,
          timestamp,
          expiration,
          matcherFee,
          priceMode = OrderPriceMode.Default
        )
        .explicitGet()
      val buyFee  = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue
      val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue
      val trans =
        ExchangeTransaction
          .signed(1.toByte, matcher.privateKey, o1, o2, matchedAmount, price, buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100)
          .explicitGet()

      trans
    }

  private type OrderConstructor = (KeyPair, PublicKey, AssetPair, Long, Long, Long, Long, Long) => Order

  def exchangeV2GeneratorP(
      buyer: KeyPair,
      seller: KeyPair,
      amountAssetId: Asset,
      priceAssetId: Asset,
      fixedMatcherFee: Option[Long] = None,
      orderVersions: Set[Byte] = Set(1, 2, 3),
      buyMatcherFeeAssetId: Asset = Waves,
      sellMatcherFeeAssetId: Asset = Waves,
      fixedMatcher: Option[KeyPair] = None
  ): Gen[ExchangeTransaction] = {
    def mkBuyOrder(version: TxVersion): OrderConstructor = (version: @unchecked) match {
      case Order.V1 => Order.buy(Order.V1, _, _, _, _, _, _, _, _).explicitGet()
      case Order.V2 => Order.buy(Order.V2, _, _, _, _, _, _, _, _).explicitGet()
      case Order.V3 => Order.buy(Order.V3, _, _, _, _, _, _, _, _, buyMatcherFeeAssetId).explicitGet()
    }

    def mkSellOrder(version: TxVersion): OrderConstructor = (version: @unchecked) match {
      case Order.V1 => Order.sell(Order.V1, _, _, _, _, _, _, _, _).explicitGet()
      case Order.V2 => Order.sell(Order.V2, _, _, _, _, _, _, _, _).explicitGet()
      case Order.V3 => Order.sell(Order.V3, _, _, _, _, _, _, _, _, sellMatcherFeeAssetId).explicitGet()
    }

    for {
      (_, generatedMatcher, _, _, amount1, price, timestamp, expiration, generatedMatcherFee) <- orderParamGen
      amount2: Long                                                                           <- matcherAmountGen
      matcher    = fixedMatcher.getOrElse(generatedMatcher)
      matcherFee = fixedMatcherFee.getOrElse(generatedMatcherFee)
      matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
      assetPair = AssetPair(amountAssetId, priceAssetId)
      mkO1 <- Gen.oneOf(orderVersions.map(mkBuyOrder).toSeq)
      mkO2 <- Gen.oneOf(orderVersions.map(mkSellOrder).toSeq)
    } yield {

      val buyFee  = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue
      val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue

      val o1 = mkO1(buyer, matcher.publicKey, assetPair, amount1, price, timestamp, expiration, matcherFee)
      val o2 = mkO2(seller, matcher.publicKey, assetPair, amount2, price, timestamp, expiration, matcherFee)

      ExchangeTransaction
        .signed(2.toByte, matcher.privateKey, o1, o2, matchedAmount, price, buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100)
        .explicitGet()
    }
  }

  val randomTransactionGen: Gen[Transaction & ProvenTransaction] = (for {
    tr <- transferV1Gen
    (is, ri, bu) <- issueReissueBurnGen.retryUntil { case (i, r, b) =>
      i.version == 1 && r.version == 1 && b.version == 1
    }
    ca <- createAliasGen.retryUntil(_.version == 1)
    xt <- exchangeTransactionGen
    tx <- Gen.oneOf(tr, is, ri, ca, bu, xt)
  } yield tx).label("random transaction")

  def randomTransactionsGen(count: Int): Gen[Seq[Transaction]] =
    for {
      transactions <- Gen.listOfN(count, randomTransactionGen)
    } yield transactions

  def genesisGeneratorP(recipient: Address): Gen[GenesisTransaction] =
    for {
      amt <- Gen.choose(1, 100000000L * 100000000L)
      ts  <- positiveIntGen
    } yield GenesisTransaction.create(recipient, amt, ts).explicitGet()

  import DataEntry.MaxKeySize

  val dataKeyGen: Gen[String] = for {
    size <- Gen.choose(1, MaxKeySize)
  } yield Random.nextString(size)

  val dataScriptsKeyGen: Gen[String] = for {
    size <- Gen.choose(1, 10)
  } yield Random.nextString(size)

  def longEntryGen(keyGen: Gen[String] = dataKeyGen): Gen[IntegerDataEntry] =
    for {
      key   <- keyGen
      value <- Gen.choose[Long](Long.MinValue, Long.MaxValue)
    } yield IntegerDataEntry(key, value)

  def booleanEntryGen(keyGen: Gen[String] = dataKeyGen): Gen[BooleanDataEntry] =
    for {
      key   <- keyGen
      value <- Gen.oneOf(true, false)
    } yield BooleanDataEntry(key, value)

  def binaryEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[BinaryDataEntry] =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- byteArrayGen(size)
    } yield BinaryDataEntry(key, ByteStr(value))

  def stringEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen): Gen[StringDataEntry] =
    for {
      key   <- keyGen
      size  <- Gen.choose(0, maxSize)
      value <- Gen.listOfN(size, aliasAlphabetGen)
    } yield StringDataEntry(key, value.mkString)

  def emptyEntryGen(keyGen: Gen[String] = dataKeyGen): Gen[EmptyDataEntry] =
    for (key <- keyGen) yield EmptyDataEntry(key)

  def dataEntryGen(maxSize: Int, keyGen: Gen[String] = dataKeyGen, withDeleteEntry: Boolean = false): Gen[DataEntry[?]] =
    Gen.oneOf(
      longEntryGen(keyGen),
      booleanEntryGen(keyGen),
      (Seq(
        binaryEntryGen(maxSize, keyGen),
        stringEntryGen(maxSize, keyGen)
      ) ++ (if (withDeleteEntry) Seq(emptyEntryGen(keyGen)) else Seq()))*
    )

  val dataTransactionGen: Gen[DataTransaction] = dataTransactionGen(DataTransaction.MaxEntryCount)

  def dataTransactionGen(
      maxEntryCount: Int,
      useForScript: Boolean = false,
      withDeleteEntry: Boolean = false,
      sender: Option[SeedKeyPair] = None
  ): Gen[DataTransaction] =
    (for {
      timestamp <- timestampGen
      sender    <- sender.fold(accountGen)(Gen.const)
      size      <- Gen.choose(0, maxEntryCount)
      maxEntrySize = if (useForScript) 200 else (DataTransaction.MaxBytes - 122) / (size max 1) min DataEntry.MaxValueSize
      data <-
        if (useForScript) Gen.listOfN(size, dataEntryGen(maxEntrySize, dataScriptsKeyGen, withDeleteEntry))
        else Gen.listOfN(size, dataEntryGen(maxEntrySize))
      uniq = data.foldRight(List.empty[DataEntry[?]]) { (e, es) =>
        if (es.exists(_.key == e.key)) es else e :: es
      }
    } yield DataTransaction.selfSigned(if (withDeleteEntry) 2.toByte else 1.toByte, sender, uniq, 15000000, timestamp).explicitGet())
      .label("DataTransaction")

  def issueV2TransactionGen(
      senderGen: Gen[KeyPair] = accountGen,
      _scriptGen: Gen[Option[Script]] = Gen.option(scriptGen),
      reissuableParam: Option[Boolean] = None,
      quantityParam: Option[Long] = None,
      feeParam: Option[Long] = None
  ): Gen[IssueTransaction] =
    for {
      script                                                                                                 <- _scriptGen
      sender                                                                                                 <- senderGen
      (_, assetName, description, generatedQuantity, decimals, generatedReissuable, generatedFee, timestamp) <- issueParamGen
      reissuable = reissuableParam.getOrElse(generatedReissuable)
      quantity   = quantityParam.getOrElse(generatedQuantity)
      fee        = feeParam.getOrElse(generatedFee)
    } yield IssueTransaction
      .selfSigned(TxVersion.V2, sender, new String(assetName), new String(description), quantity, decimals, reissuable, script, fee, timestamp)
      .explicitGet()

  def invokeExpressionTransactionGen(sender: KeyPair, script: ExprScript, feeAmount: Long): Gen[InvokeExpressionTransaction] =
    InvokeExpressionTransaction.selfSigned(1, sender, script, feeAmount, Waves, ntpTime.getTimestamp()).explicitGet()
}

trait TransactionGen extends TransactionGenBase { _: Suite => }

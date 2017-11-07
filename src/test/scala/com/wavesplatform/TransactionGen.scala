package com.wavesplatform

import com.wavesplatform.settings.Constants
import com.wavesplatform.state2._
import org.scalacheck.Gen.{alphaLowerChar, frequency, numChar, alphaUpperChar}
import org.scalacheck.{Arbitrary, Gen}
import scorex.account.PublicKeyAccount._
import scorex.account._
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils.NTP

trait TransactionGen {

  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])

  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val bytes64gen: Gen[Array[Byte]] = byteArrayGen(64)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = for {
    length <- Gen.chooseNum(minSize, maxSize)
    bytes <- byteArrayGen(length)
  } yield bytes

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray) }
  }

  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(NTP.correctedTime() - _)

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))

  val aliasSymbolChar: Gen[Char] = Gen.oneOf('.','@', '_', '-')

  val aliasAlphabetGen: Gen[Char] = frequency((1, numChar), (1, aliasSymbolChar), (9, alphaLowerChar))

  val invalidAliasAlphabetGen: Gen[Char] = frequency((1, numChar), (1, aliasSymbolChar), (9, alphaUpperChar))

  val validAliasStringGen: Gen[String] = for {
    length <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, aliasAlphabetGen)
  } yield aliasChars.mkString

  val aliasGen: Gen[Alias] = for {
    str <- validAliasStringGen
  } yield Alias.buildWithCurrentNetworkByte(str.mkString).explicitGet()

  val invalidAliasStringGen: Gen[String] = for {
    length <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    aliasChars <- Gen.listOfN(length, invalidAliasAlphabetGen)
  } yield aliasChars.mkString

  val accountOrAliasGen: Gen[AddressOrAlias] = Gen.oneOf(aliasGen, accountGen.map(PublicKeyAccount.toAddress(_)))

  def otherAccountGen(candidate: PrivateKeyAccount): Gen[PrivateKeyAccount] = accountGen.flatMap(Gen.oneOf(candidate, _))

  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue / 100)
  val positiveIntGen: Gen[Int] = Gen.choose(1, Int.MaxValue / 100)
  val smallFeeGen: Gen[Long] = Gen.choose(1, 100000000)

  val maxOrderTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - 100)

  val wavesAssetGen: Gen[Option[ByteStr]] = Gen.const(None)
  val assetIdGen: Gen[Option[ByteStr]] = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen.map(ByteStr(_)))))

  val assetPairGen = assetIdGen.flatMap {
    case None => bytes32gen.map(b => AssetPair(None, Some(ByteStr(b))))
    case a1@Some(a1bytes) =>
      val a2bytesGen = byteArrayGen(31).map(a2bytes => Option((~a1bytes.arr(0)).toByte +: a2bytes))
      Gen.oneOf(Gen.const(None), a2bytesGen).map(a2 => AssetPair(a1, a2.map(ByteStr(_))))
  }

  val paymentGen: Gen[PaymentTransaction] = for {
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    tx <- paymentGeneratorP(sender, recipient)
  } yield tx

  val selfPaymentGen: Gen[PaymentTransaction] = accountGen.flatMap(acc => paymentGeneratorP(acc, acc))

  def paymentGeneratorP(sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] =
    timestampGen.flatMap(ts => paymentGeneratorP(ts, sender, recipient))

  def paymentGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] = for {
    amount: Long <- positiveLongGen
    fee: Long <- smallFeeGen
  } yield PaymentTransaction.create(sender, recipient, amount, fee, timestamp).right.get


  private val leaseParamGen = for {
    sender <- accountGen
    amount <- positiveLongGen
    fee <- smallFeeGen
    timestamp <- timestampGen
    recipient <- accountGen
  } yield (sender, amount, fee, timestamp, recipient)

  val leaseAndCancelGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    lease = LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get
    cancelFee <- smallFeeGen
  } yield (lease, LeaseCancelTransaction.create(sender, lease.id(), cancelFee, timestamp + 1).right.get)

  def leaseAndCancelGeneratorP(leaseSender: PrivateKeyAccount, recipient: AddressOrAlias, unleaseSender: PrivateKeyAccount): Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (_, amount, fee, timestamp, _) <- leaseParamGen
    lease = LeaseTransaction.create(leaseSender, amount, fee, timestamp, recipient).right.get
    fee2 <- smallFeeGen
    unlease = LeaseCancelTransaction.create(unleaseSender, lease.id(), fee2, timestamp + 1).right.get
  } yield (lease, unlease)

  val twoLeasesGen: Gen[(LeaseTransaction, LeaseTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    amount2 <- positiveLongGen
    recipient2: PrivateKeyAccount <- accountGen
    fee2 <- smallFeeGen
  } yield (LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get,
    LeaseTransaction.create(sender, amount2, fee2, timestamp + 1, recipient2).right.get)

  val leaseAndCancelWithOtherSenderGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    otherSender: PrivateKeyAccount <- accountGen
    lease = LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get
    fee2 <- smallFeeGen
    timestamp2 <- positiveLongGen
  } yield (lease, LeaseCancelTransaction.create(otherSender, lease.id(), fee2, timestamp2).right.get)

  val leaseGen: Gen[LeaseTransaction] = leaseAndCancelGen.map(_._1)
  val leaseCancelGen: Gen[LeaseCancelTransaction] = leaseAndCancelGen.map(_._2)

  private val transferParamGen = for {
    amount <- positiveLongGen
    feeAmount <- smallFeeGen
    assetId <- Gen.option(bytes32gen)
    feeAssetId <- Gen.option(bytes32gen)
    timestamp <- timestampGen
    sender <- accountGen
    attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient <- accountOrAliasGen
  } yield (assetId.map(ByteStr(_)), sender, recipient, amount, timestamp, feeAssetId.map(ByteStr(_)), feeAmount, attachment)

  def transferGeneratorP(sender: PrivateKeyAccount, recipient: AddressOrAlias,
                         assetId: Option[AssetId], feeAssetId: Option[AssetId]): Gen[TransferTransaction] = for {
    (_, _, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).right.get

  def transferGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: AddressOrAlias,
                         assetId: Option[AssetId], feeAssetId: Option[AssetId]): Gen[TransferTransaction]  = for {
    (_, _, _, amount, _, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).right.get

  val transferGen = (for {
    (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).right.get)
    .label("transferTransaction")

  val transferWithWavesFeeGen = for {
    (assetId, sender, recipient, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, None, feeAmount, attachment).right.get

  val selfTransferWithWavesFeeGen: Gen[TransferTransaction] = for {
    (assetId, sender, _, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.create(assetId, sender, sender, amount, timestamp, None, feeAmount, attachment).right.get

  val selfTransferGen: Gen[TransferTransaction] = for {
    (assetId, sender, _, amount, timestamp, feeAssetId, feeAmount, attachment) <- transferParamGen
  } yield TransferTransaction.create(assetId, sender, sender, amount, timestamp, feeAssetId, feeAmount, attachment).right.get

  val MinIssueFee = 100000000

  val createAliasGen: Gen[CreateAliasTransaction] = for {
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    alias: Alias <- aliasGen
  } yield CreateAliasTransaction.create(sender, alias, MinIssueFee, timestamp).right.get

  val issueParamGen = for {
    sender: PrivateKeyAccount <- accountGen
    assetName <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
    quantity <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
    decimals <- Gen.choose(0: Byte, 8: Byte)
    reissuable <- Arbitrary.arbitrary[Boolean]
    fee <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp <- positiveLongGen
  } yield (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp)

  val issueReissueBurnGen: Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    amount <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    r <- issueReissueBurnGeneratorP(amount, amount, amount, sender)
  } yield r

  def issueReissueBurnGeneratorP(issueQuantity: Long, sender: PrivateKeyAccount): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] =
    issueReissueBurnGeneratorP(issueQuantity, issueQuantity, issueQuantity, sender)

  def issueReissueBurnGeneratorP(issueQuantity: Long, reissueQuantity: Long, burnQuantity: Long, sender: PrivateKeyAccount): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    (_, assetName, description, _, decimals, reissuable, iFee, timestamp) <- issueParamGen
    burnAmount <- Gen.choose(0L, burnQuantity)
    reissuable2 <- Arbitrary.arbitrary[Boolean]
    fee <- smallFeeGen
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, issueQuantity, decimals, reissuable, iFee, timestamp).right.get
    val reissue = ReissueTransaction.create(sender, issue.assetId(), reissueQuantity, reissuable2, fee, timestamp).right.get
    val burn = BurnTransaction.create(sender, issue.assetId(), burnAmount, fee, timestamp).right.get
    (issue, reissue, burn)
  }

  val issueWithInvalidReissuesGen: Gen[(IssueTransaction, ReissueTransaction, ReissueTransaction)] = for {
    (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
    fee <- smallFeeGen
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp).right.get
    val reissue1 = ReissueTransaction.create(sender, issue.assetId(), quantity, reissuable = false, fee, timestamp).right.get
    val reissue2 = ReissueTransaction.create(sender, issue.assetId(), quantity, reissuable = true, fee, timestamp + 1).right.get
    (issue, reissue1, reissue2)
  }

  def issueGen(sender: PrivateKeyAccount, fixedQuantity: Option[Long] = None): Gen[IssueTransaction] = for {
    (_, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
  } yield {
    IssueTransaction.create(sender, assetName, description, fixedQuantity.getOrElse(quantity), decimals, reissuable = false, 1*Constants.UnitsInWave, timestamp).right.get
  }

  val issueGen: Gen[IssueTransaction] = issueReissueBurnGen.map(_._1)
  val reissueGen: Gen[ReissueTransaction] = issueReissueBurnGen.map(_._2)
  val burnGen: Gen[BurnTransaction] = issueReissueBurnGen.map(_._3)

  val priceGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherAmountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)
  val matcherFeeAmountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)

  val orderTypeGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderParamGen = for {
    sender <- accountGen
    matcher <- accountGen
    pair <- assetPairGen
    orderType <- orderTypeGen
    price <- priceGen
    amount <- matcherAmountGen
    timestamp <- timestampGen
    expiration <- maxOrderTimeGen
    matcherFee <- matcherFeeAmountGen
  } yield (sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee)

  val orderGen: Gen[Order] = for {
    (sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee) <- orderParamGen
  } yield Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee)

  val arbitraryOrderGen: Gen[Order] = for {
    (sender, matcher, pair, orderType, _, _, _, _, _) <- orderParamGen
    price <- Arbitrary.arbitrary[Long]
    amount <- Arbitrary.arbitrary[Long]
    timestamp <- Arbitrary.arbitrary[Long]
    expiration <- Arbitrary.arbitrary[Long]
    matcherFee <- Arbitrary.arbitrary[Long]
  } yield Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee)

  val exchangeTransactionGen: Gen[ExchangeTransaction] = for {
    sender1: PrivateKeyAccount <- accountGen
    sender2: PrivateKeyAccount <- accountGen
    assetPair <- assetPairGen
    r <- exchangeGeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset)
  } yield r

  def exchangeGeneratorP(buyer: PrivateKeyAccount, seller: PrivateKeyAccount, amountAssetId: Option[ByteStr],
                         priceAssetId: Option[ByteStr], fixedMatcherFee: Option[Long] = None): Gen[ExchangeTransaction] = for {
    (_, matcher, _, _, price, amount1, timestamp, expiration, genMatcherFee) <- orderParamGen
    amount2: Long <- matcherAmountGen
    matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
    assetPair = AssetPair(amountAssetId, priceAssetId)
  } yield {
    val matcherFee = fixedMatcherFee.getOrElse(genMatcherFee)
    val o1 = Order.buy(buyer, matcher, assetPair, price, amount1, timestamp, expiration, matcherFee)
    val o2 = Order.sell(seller, matcher, assetPair, price, amount2, timestamp, expiration, matcherFee)
    val buyFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
    val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()
    val trans = ExchangeTransaction.create(matcher, o1, o2, price, matchedAmount,
      buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100).explicitGet()

    trans
  }

  val randomTransactionGen: Gen[SignedTransaction] = (for {
    tr <- transferGen
    (is, ri, bu) <- issueReissueBurnGen
    ca <- createAliasGen
    xt <- exchangeTransactionGen
    tx <- Gen.oneOf(tr, is, ri, ca, bu, xt)
  } yield tx).label("random transaction")

  def randomTransactionsGen(count: Int): Gen[Seq[SignedTransaction]] = for {
    transactions <- Gen.listOfN(count, randomTransactionGen)
  } yield transactions

  val genesisGen: Gen[GenesisTransaction] = accountGen.flatMap(genesisGeneratorP)

  def genesisGeneratorP(recipient: PrivateKeyAccount): Gen[GenesisTransaction] = for {
    amt <- positiveLongGen
    ts <- positiveIntGen
  } yield GenesisTransaction.create(recipient, amt, ts).right.get

}

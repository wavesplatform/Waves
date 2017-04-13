package com.wavesplatform

import org.scalacheck.{Arbitrary, Gen}
import scorex.account.PublicKeyAccount._
import scorex.account._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{CreateAliasTransaction, PaymentTransaction}
import scorex.utils.{ByteArrayExtension, NTP}

trait TransactionGen {

  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.listOfN(length, Arbitrary.arbitrary[Byte]).map(_.toArray)

  val bytes32gen = byteArrayGen(32)
  val bytes64gen = byteArrayGen(64)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = for {
    length <- Gen.chooseNum(minSize, maxSize)
    bytes <- byteArrayGen(length)
  } yield bytes

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray) }
  }

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val aliasGen: Gen[Alias] = for {
    l <- Gen.chooseNum(Alias.MinLength, Alias.MaxLength)
    alias <- Gen.listOfN(l, Gen.alphaNumChar)
  } yield Alias.buildWithCurrentNetworkByte(alias.mkString).right.get

  val accountOrAliasGen: Gen[AccountOrAlias] = Gen.oneOf(aliasGen, accountGen.map(PublicKeyAccount.toAddress(_)))

  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue / 3)
  val positiveIntGen: Gen[Int] = Gen.choose(1, Int.MaxValue / 3)
  val smallFeeGen: Gen[Long] = Gen.choose(1, 100000000)

  val maxOrderTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
  val timestampGen: Gen[Long] = Gen.choose(1, 1000).map(NTP.correctedTime() - _)

  val wavesAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]] = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen)))

  val assetPairGen = for {
    a1 <- assetIdGen
    a2 <- assetIdGen
    if !ByteArrayExtension.sameOption(a1, a2)
  } yield AssetPair(a1, a2)

  private val paymentParamGen: Gen[(PrivateKeyAccount, PublicKeyAccount, Long, Long, Long)] = for {
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    amount: Long <- Gen.choose(0, Long.MaxValue)
    fee: Long <- Gen.choose(0, Long.MaxValue - amount - 1)
    timestamp: Long <- positiveLongGen
  } yield (sender, recipient, amount, fee, timestamp)

  val paymentGen: Gen[PaymentTransaction] = for {
    (sender, recipient, amount, fee, timestamp) <- paymentParamGen
  } yield PaymentTransaction.create(sender, recipient, amount, fee, timestamp).right.get

  val selfPaymentGen: Gen[PaymentTransaction] = for {
    (sender, _, amount, fee, timestamp) <- paymentParamGen
  } yield PaymentTransaction.create(sender, sender, amount, fee, timestamp).right.get

  private val leaseParamGen = for {
    sender <- accountGen
    amount <- positiveLongGen
    fee <- smallFeeGen
    timestamp <- positiveLongGen
    recipient <- accountOrAliasGen
  } yield (sender, amount, fee, timestamp, recipient)

  val leaseAndCancelGen: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    (sender, amount, fee, timestamp, recipient) <- leaseParamGen
    lease = LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get
    cancelFee <- smallFeeGen
  } yield (lease, LeaseCancelTransaction.create(sender, lease.id, cancelFee, timestamp + 1).right.get)

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
  } yield (lease, LeaseCancelTransaction.create(otherSender, lease.id, fee2, timestamp2).right.get)

  val leaseGen: Gen[LeaseTransaction] = leaseAndCancelGen.map(_._1)
  val leaseCancelGen: Gen[LeaseCancelTransaction] = leaseAndCancelGen.map(_._2)

  private val transferParamGen = for {
    amount <- Gen.posNum[Long]
    feeAmount <- Gen.chooseNum(1, Long.MaxValue - amount - 1)
    assetId <- Gen.option(bytes32gen)
    feeAssetId <- Gen.option(bytes32gen)
    timestamp <- Gen.posNum[Long]
    sender <- accountGen
    attachment <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient <- accountOrAliasGen
  } yield (assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment)

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
    quantity <- positiveLongGen
    decimals <- Gen.choose(0: Byte, 8: Byte)
    reissuable <- Arbitrary.arbitrary[Boolean]
    fee <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp <- positiveLongGen
  } yield (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp)

  val issueReissueGen: Gen[(IssueTransaction, IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    (sender, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
    burnAmount <- Gen.choose(0L, quantity)
    reissuable2 <- Arbitrary.arbitrary[Boolean]
    fee <- Gen.choose(1L, 2000000L)
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable, iFee, timestamp).right.get
    val issue2 = IssueTransaction.create(sender, assetName, description, quantity, decimals,
      reissuable, iFee, Math.max(timestamp, 1476459220001L)).right.get
    val reissue = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable2, fee, timestamp).right.get
    val burn = BurnTransaction.create(sender, issue.assetId, burnAmount, fee, timestamp).right.get
    (issue, issue2, reissue, burn)
  }

  val issueWithInvalidReissuesGen: Gen[(IssueTransaction, ReissueTransaction, ReissueTransaction)] = for {
    (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
    fee <- Gen.choose(1L, 2000000L)
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp).right.get
    val reissue1 = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable = false, fee, timestamp).right.get
    val reissue2 = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable = true, fee, timestamp + 1).right.get
    (issue, reissue1, reissue2)
  }

  val issueGen: Gen[IssueTransaction] = issueReissueGen.map(_._1)
  val reissueGen: Gen[ReissueTransaction] = issueReissueGen.map(_._3)
  val burnGen: Gen[BurnTransaction] = issueReissueGen.map(_._4)

  val priceGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)
  val amountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)
  val feeAmountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)

  val orderTypeGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderParamGen = for {
    sender <- accountGen
    matcher <- accountGen
    pair <- assetPairGen
    orderType <- orderTypeGen
    price <- priceGen
    amount <- amountGen
    timestamp <- timestampGen
    expiration <- maxOrderTimeGen
    matcherFee <- feeAmountGen
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
    (sender1, matcher, assetPair, _, price, amount1, timestamp, expiration, matcherFee) <- orderParamGen
    sender2 <- accountGen
    amount2 <- amountGen
    matchedAmount <- Gen.choose(Math.min(amount1, amount2) / 2, Math.min(amount1, amount2))
  } yield {
    val o1 = Order.buy(sender1, matcher, assetPair, price, amount1, timestamp, expiration, matcherFee)
    val o2 = Order.sell(sender2, matcher, assetPair, price, amount2, timestamp, expiration, matcherFee)
    val buyFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
    val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()
    val trans = ExchangeTransaction.create(matcher, o1, o2, price, matchedAmount,
      buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100).right.get

    trans
  }

  val randomTransactionGen = (for {
    tr <- transferGen
//    (is, _, ri, bu) <- issueReissueGen
//    ca <- createAliasGen
//    xt <- exchangeTransactionGen
//    tx <- Gen.oneOf(tr, is, ri, ca, bu, xt)
  } yield tr).label("random transaction")
}

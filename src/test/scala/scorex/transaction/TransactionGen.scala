package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.enablers.Containing
import org.scalatest.matchers.{BeMatcher, MatchResult}
import scorex.account._
import scorex.account.PublicKeyAccount._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils.{ByteArrayExtension, NTP}

trait TransactionGen {

  val MAX_LONG = Long.MaxValue / 3
  val MAX_INT = Int.MaxValue / 3

  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val bytes64gen: Gen[Array[Byte]] = Gen.listOfN(64, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray) }
  }

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val aliasGen: Gen[Alias] = genBoundedString(Alias.MinLength, Alias.MaxLength)
    .map(ar => new String(ar))
    .suchThat(!_.contains("\n"))
    .suchThat(s => s.trim == s)
    .map(Alias.buildWithCurrentNetworkByte(_).right.get)

  val accountOrAliasGen: Gen[AccountOrAlias] = Gen.oneOf(aliasGen, accountGen.map(PublicKeyAccount.toAccount(_)))

  val positiveLongGen: Gen[Long] = Gen.choose(1, MAX_LONG)
  val positiveIntGen: Gen[Int] = Gen.choose(1, MAX_INT)
  val smallFeeGen: Gen[Long] = Gen.choose(1, 100000000)

  val maxTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
  val timestampGen: Gen[Long] = Gen.choose(1, 1000).map(NTP.correctedTime() - _)

  val wavesAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]] = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen)))

  val assetPairGen = Gen.zip(assetIdGen, assetIdGen).
    suchThat(p => !ByteArrayExtension.sameOption(p._1, p._2)).
    map(p => AssetPair(p._1, p._2))

  val paymentGenerator: Gen[PaymentTransaction] = for {
    amount: Long <- Gen.choose(0, MAX_LONG)
    fee: Long <- Gen.choose(0, MAX_LONG - amount - 1)
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
  } yield PaymentTransaction.create(sender, recipient, amount, fee, timestamp).right.get

  val leaseAndCancelGenerator: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    amount <- positiveLongGen
    fee <- smallFeeGen
    timestamp <- positiveLongGen
    recipient: AccountOrAlias <- accountOrAliasGen
    lease = LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get
    fee2 <- smallFeeGen
    unlease = LeaseCancelTransaction.create(sender, lease.id, fee2, timestamp + 1).right.get
  } yield (lease, unlease)

  val twoLeasesGenerator: Gen[(LeaseTransaction, LeaseTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    amount <- positiveLongGen
    amount2 <- positiveLongGen
    fee <- smallFeeGen
    timestamp <- positiveLongGen
    recipient: PrivateKeyAccount <- accountGen
    recipient2: PrivateKeyAccount <- accountGen
    lease = LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get
    fee2 <- smallFeeGen
    lease2 = LeaseTransaction.create(sender, amount2, fee2, timestamp + 1, recipient2).right.get
  } yield (lease, lease2)

  val leaseAndCancelWithOtherSenderGenerator: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    otherSender: PrivateKeyAccount <- accountGen
    amount <- positiveLongGen
    fee <- smallFeeGen
    timestamp <- positiveLongGen
    recipient: AccountOrAlias <- accountOrAliasGen
    lease = LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get
    fee2 <- smallFeeGen
    timestamp2 <- positiveLongGen
    unlease = LeaseCancelTransaction.create(otherSender, lease.id, fee2, timestamp2).right.get
  } yield (lease, unlease)

  val leaseGenerator: Gen[LeaseTransaction] = leaseAndCancelGenerator.map(_._1)
  val leaseCancelGenerator: Gen[LeaseCancelTransaction] = leaseAndCancelGenerator.map(_._2)

  val selfPaymentGenerator: Gen[PaymentTransaction] = for {
    account: PrivateKeyAccount <- accountGen
    amount: Long <- Gen.choose(0, MAX_LONG)
    fee: Long <- smallFeeGen
    timestamp: Long <- positiveLongGen
  } yield PaymentTransaction.create(account, account, amount, fee, timestamp).right.get

  val transferGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, MAX_LONG)
    feeAmount: Long <- Gen.choose(0, MAX_LONG - amount - 1)
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    feeAssetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient: AccountOrAlias <- accountOrAliasGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).right.get


  val transferGeneratorWithNoneFeeAssetId: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, MAX_LONG)
    feeAmount: Long <- Gen.choose(0, MAX_LONG - amount - 1)
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient: AccountOrAlias <- accountOrAliasGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, None, feeAmount, attachment).right.get

  val MinIssueFee = 100000000

  val createAliasGenerator: Gen[CreateAliasTransaction] = for {
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    alias: Alias <- aliasGen
  } yield CreateAliasTransaction.create(sender, alias, MinIssueFee, timestamp).right.get

  val selfTransferGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, MAX_LONG)
    feeAmount: Long <- smallFeeGen
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    feeAssetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- positiveLongGen
    account: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
  } yield TransferTransaction.create(assetId, account, account, amount, timestamp, feeAssetId, feeAmount, attachment).right.get

  val selfTransferWithWavesFeeGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, MAX_LONG)
    feeAmount: Long <- smallFeeGen
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- positiveLongGen
    account: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
  } yield TransferTransaction.create(assetId, account, account, amount, timestamp, None, feeAmount, attachment).right.get

  val priceGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)
  val amountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)
  val feeAmountGen: Gen[Long] = Gen.choose(1, 3 * 100000L * 100000000L)

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    pair: AssetPair <- assetPairGen
    orderType: OrderType <- orderTypeGenerator
    price: Long <- priceGen
    amount: Long <- amountGen
    timestamp: Long <- timestampGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- feeAmountGen
  } yield (Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee), sender)


  val issueReissueGenerator: Gen[(IssueTransaction, IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    assetName <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
    quantity <- positiveLongGen
    burnAmount <- Gen.choose(0L, quantity)
    decimals <- Gen.choose(0: Byte, 8: Byte)
    reissuable <- Arbitrary.arbitrary[Boolean]
    reissuable2 <- Arbitrary.arbitrary[Boolean]
    fee <- Gen.choose(1L, 2000000L)
    iFee <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp <- positiveLongGen
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable, iFee, timestamp).right.get
    val issue2 = IssueTransaction.create(sender, assetName, description, quantity, decimals,
      reissuable, iFee, Math.max(timestamp, 1476459220001L)).right.get
    val reissue = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable2, fee, timestamp).right.get
    val burn = BurnTransaction.create(sender, issue.assetId, burnAmount, fee, timestamp).right.get
    (issue, issue2, reissue, burn)
  }

  val issueWithInvalidReissuesGenerator: Gen[(IssueTransaction, ReissueTransaction, ReissueTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    assetName <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
    quantity <- positiveLongGen
    decimals <- Gen.choose(0: Byte, 8: Byte)
    fee <- Gen.choose(1L, 2000000L)
    iFee <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp <- positiveLongGen
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp).right.get
    val reissue1 = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable = false, fee, timestamp).right.get
    val reissue2 = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable = true, fee, timestamp + 1).right.get
    (issue, reissue1, reissue2)
  }

  val issueGenerator: Gen[IssueTransaction] = issueReissueGenerator.map(_._1)
  val reissueGenerator: Gen[ReissueTransaction] = issueReissueGenerator.map(_._3)
  val burnGenerator: Gen[BurnTransaction] = issueReissueGenerator.map(_._4)

  val invalidOrderGenerator: Gen[Order] = for {
    sender: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    orderType <- orderTypeGenerator
    price: Long <- Arbitrary.arbitrary[Long]
    amount: Long <- Arbitrary.arbitrary[Long]
    timestamp: Long <- Arbitrary.arbitrary[Long]
    expiration: Long <- Arbitrary.arbitrary[Long]
    matcherFee: Long <- Arbitrary.arbitrary[Long]
  } yield Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee)

  val orderMatchGenerator: Gen[(ExchangeTransaction, PrivateKeyAccount)] = for {
    sender1: PrivateKeyAccount <- accountGen
    sender2: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    assetPair <- assetPairGen
    spendAssetID: Array[Byte] <- bytes32gen
    receiveAssetID: Array[Byte] <- bytes32gen
    price: Long <- priceGen
    amount1: Long <- amountGen
    amount2: Long <- amountGen
    matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2, Math.min(amount1, amount2))
    timestamp: Long <- timestampGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- feeAmountGen
  } yield {
    val o1 = Order.buy(sender1, matcher, assetPair, price, amount1, timestamp, expiration, matcherFee)
    val o2 = Order.sell(sender2, matcher, assetPair, price, amount2, timestamp, expiration, matcherFee)
    val buyFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
    val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()
    val trans = ExchangeTransaction.create(matcher, o1, o2, price, matchedAmount,
      buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100).right.get

    (trans, matcher)
  }

  implicit val orderMatchArb: Arbitrary[(ExchangeTransaction, PrivateKeyAccount)] = Arbitrary {
    orderMatchGenerator
  }
  implicit val orderArb: Arbitrary[(Order, PrivateKeyAccount)] = Arbitrary {
    orderGenerator
  }
  implicit val privateKeyAccArb: Arbitrary[PrivateKeyAccount] = Arbitrary {
    accountGen
  }

  implicit val containingNatureOfValidation: Containing[Validation] =
    new Containing[Validation] {
      def contains(v: Validation, ele: Any): Boolean =
        !v.status && v.labels.contains(ele.toString)

      def containsOneOf(v: Validation, elements: scala.collection.Seq[Any]): Boolean = {
        !v.status && elements.map(_.toString).map(v.labels.contains).reduce(_ || _)
      }

      def containsNoneOf(v: Validation, elements: scala.collection.Seq[Any]): Boolean = {
        v.status || elements.map(_.toString).map(v.labels.contains).reduce(!_ && !_)
      }
    }

  class ValidationMatcher extends BeMatcher[Validation] {
    def apply(left: Validation): MatchResult =
      MatchResult(
        left.status,
        left.toString + " was invalid",
        left.toString + " was valid"
      )
  }

  val valid = new ValidationMatcher

}

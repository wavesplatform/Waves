package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.enablers.Containing
import org.scalatest.matchers.{BeMatcher, MatchResult}
import scorex.account.PublicKeyAccount._
import scorex.account._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils.{ByteArrayExtension, NTP}

trait TransactionGen {

  val BIG_LONG = Long.MaxValue / 100
  val BIG_INT = Int.MaxValue / 100

  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val bytes64gen: Gen[Array[Byte]] = Gen.listOfN(64, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  def genBoundedString(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Gen.choose(0, 0x7f).map(_.toByte)).map(_.toArray) }
  }

  def nelMax[T](g: Gen[T], max: Int = 10): Gen[List[T]] = Gen.choose(1, max).flatMap(Gen.listOfN(_, g))

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))

  val aliasGen: Gen[Alias] = genBoundedString(Alias.MinLength, Alias.MaxLength)
    .map(ar => new String(ar))
    .suchThat(!_.contains("\n"))
    .suchThat(s => s.trim == s)
    .map(Alias.buildWithCurrentNetworkByte(_).right.get)

  def otherAccountGen(candidate: PrivateKeyAccount): Gen[PrivateKeyAccount] = accountGen.flatMap(Gen.oneOf(candidate, _))

  val accountOrAliasGen: Gen[AccountOrAlias] = Gen.oneOf(aliasGen, accountGen.map(PublicKeyAccount.toAccount(_)))

  val positiveLongGen: Gen[Long] = Gen.choose(1, BIG_LONG)
  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - 100)
  val positiveIntGen: Gen[Int] = Gen.choose(1, BIG_INT)
  val smallFeeGen: Gen[Long] = Gen.choose(1, 100000000)

  val maxTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(NTP.correctedTime() - _)

  val wavesAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]] = Gen.frequency((1, wavesAssetGen), (10, Gen.option(bytes32gen)))

  val assetPairGen: Gen[AssetPair] = Gen.zip(assetIdGen, assetIdGen).
    suchThat(p => !ByteArrayExtension.sameOption(p._1, p._2)).
    map(p => AssetPair(p._1, p._2))

  val genesisGenerator: Gen[GenesisTransaction] = accountGen.flatMap(genesisGeneratorP)

  def genesisGeneratorP(recipient: PrivateKeyAccount): Gen[GenesisTransaction] = for {
    amt <- positiveLongGen
    ts <- positiveIntGen
  } yield GenesisTransaction.create(recipient, amt, ts).right.get

  def paymentGeneratorP(sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] =
    timestampGen.flatMap(paymentGeneratorP(_, sender, recipient))

  def paymentGeneratorP(timestamp: Long, sender: PrivateKeyAccount, recipient: PrivateKeyAccount): Gen[PaymentTransaction] = for {
    amount: Long <- Gen.choose(0, BIG_LONG)
    fee: Long <- smallFeeGen
  } yield PaymentTransaction.create(sender, recipient, amount, fee, timestamp).right.get


  val paymentGenerator: Gen[PaymentTransaction] = for {
    sender <- accountGen
    rec <- accountGen
    r <- paymentGeneratorP(sender, rec)
  } yield r


  def leaseAndCancelGeneratorP(leaseSender: PrivateKeyAccount, recipient: AccountOrAlias, unleaseSender: PrivateKeyAccount): Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    amount <- positiveLongGen
    fee <- smallFeeGen
    timestamp <- timestampGen
    lease = LeaseTransaction.create(leaseSender, amount, fee, timestamp, recipient).right.get
    fee2 <- smallFeeGen
    unlease = LeaseCancelTransaction.create(unleaseSender, lease.id, fee2, timestamp + 1).right.get
  } yield (lease, unlease)

  val leaseAndCancelGenerator: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    r <- leaseAndCancelGeneratorP(sender, recipient, sender)
  } yield r

  val twoLeasesGenerator: Gen[(LeaseTransaction, LeaseTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    amount <- positiveLongGen
    amount2 <- positiveLongGen
    fee <- smallFeeGen
    timestamp <- timestampGen
    recipient: PrivateKeyAccount <- accountGen
    recipient2: PrivateKeyAccount <- accountGen
    lease = LeaseTransaction.create(sender, amount, fee, timestamp, recipient).right.get
    fee2 <- smallFeeGen
    lease2 = LeaseTransaction.create(sender, amount2, fee2, timestamp + 1, recipient2).right.get
  } yield (lease, lease2)

  val leaseAndCancelWithOtherSenderGenerator: Gen[(LeaseTransaction, LeaseCancelTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    unleaseSender: PrivateKeyAccount <- accountGen
    r <- leaseAndCancelGeneratorP(sender, recipient, unleaseSender)
  } yield r

  val leaseGenerator: Gen[LeaseTransaction] = leaseAndCancelGenerator.map(_._1)

  val leaseCancelGenerator: Gen[LeaseCancelTransaction] = leaseAndCancelGenerator.map(_._2)

  val selfPaymentGenerator: Gen[PaymentTransaction] = for {
    account: PrivateKeyAccount <- accountGen
    amount: Long <- Gen.choose(0, BIG_LONG)
    fee: Long <- smallFeeGen
    timestamp: Long <- timestampGen
  } yield PaymentTransaction.create(account, account, amount, fee, timestamp).right.get

  val transferGenerator: Gen[TransferTransaction] = for {
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    feeAssetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    account: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
    tx <- transferGeneratorP(account, recipient, assetId, feeAssetId)
  } yield tx


  def transferGeneratorP(sender: PrivateKeyAccount, recipient: AccountOrAlias,
                         assetId: Option[AssetId], feeAssetId: Option[AssetId]): Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, BIG_LONG)
    feeAmount: Long <- smallFeeGen
    timestamp: Long <- timestampGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).right.get


  val transferGeneratorWithNoneFeeAssetId: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, BIG_LONG)
    feeAmount: Long <- Gen.choose(0, BIG_LONG - amount - 1)
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- timestampGen
    sender: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient: AccountOrAlias <- accountOrAliasGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, None, feeAmount, attachment).right.get

  val MinIssueFee = 100000000

  val createAliasGenerator: Gen[CreateAliasTransaction] = for {
    timestamp: Long <- timestampGen
    sender: PrivateKeyAccount <- accountGen
    alias: Alias <- aliasGen
  } yield CreateAliasTransaction.create(sender, alias, MinIssueFee, timestamp).right.get

  val selfTransferGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, BIG_LONG)
    feeAmount: Long <- smallFeeGen
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    feeAssetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- timestampGen
    account: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
  } yield TransferTransaction.create(assetId, account, account, amount, timestamp, feeAssetId, feeAmount, attachment).right.get

  val selfTransferWithWavesFeeGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, BIG_LONG)
    feeAmount: Long <- smallFeeGen
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- timestampGen
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
    timestamp: Long <- ntpTimestampGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- feeAmountGen
  } yield (Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee), sender)


  def issueReissueGeneratorP(issueQuantity: Long, reissueQuantity: Long, burnQuantity: Long, sender: PrivateKeyAccount): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    assetName <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedString(0, IssueTransaction.MaxDescriptionLength)
    burnAmount <- Gen.choose(0L, burnQuantity)
    decimals <- Gen.choose(0: Byte, 8: Byte)
    reissuable <- Arbitrary.arbitrary[Boolean]
    reissuable2 <- Arbitrary.arbitrary[Boolean]
    fee <- Gen.choose(1L, 2000000L)
    iFee <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
    timestamp <- timestampGen
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, issueQuantity, decimals, reissuable, iFee, timestamp).right.get
    val reissue = ReissueTransaction.create(sender, issue.assetId, reissueQuantity, reissuable2, fee, timestamp).right.get
    val burn = BurnTransaction.create(sender, issue.assetId, burnAmount, fee, timestamp).right.get
    (issue, reissue, burn)
  }

  def issueReissueGeneratorP(quantity: Long, sender: PrivateKeyAccount): Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = issueReissueGeneratorP(quantity, quantity, quantity, sender)

  val issueReissueGenerator: Gen[(IssueTransaction, ReissueTransaction, BurnTransaction)] = for {
    amount <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    r <- issueReissueGeneratorP(amount, amount, amount, sender)
  } yield r

  val issueGenerator: Gen[IssueTransaction] = issueReissueGenerator.map(_._1)
  val reissueGenerator: Gen[ReissueTransaction] = issueReissueGenerator.map(_._2)
  val burnGenerator: Gen[BurnTransaction] = issueReissueGenerator.map(_._3)

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


  def exchangeGeneratorP(buyer: PrivateKeyAccount, seller: PrivateKeyAccount, amountAssetId: Option[Array[Byte]],
                         priceAssetId: Option[Array[Byte]]): Gen[(ExchangeTransaction, PrivateKeyAccount)] = for {

    matcher: PrivateKeyAccount <- accountGen
    amount1: Long <- amountGen
    amount2: Long <- amountGen
    price: Long <- priceGen
    matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2) / 2000, Math.min(amount1, amount2) / 1000)
    timestamp: Long <- ntpTimestampGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- feeAmountGen
    assetPair = AssetPair(amountAssetId, priceAssetId)
  } yield {
    val o1 = Order.buy(buyer, matcher, assetPair, price, amount1, timestamp, expiration, matcherFee)
    val o2 = Order.sell(seller, matcher, assetPair, price, amount2, timestamp, expiration, matcherFee)
    val buyFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
    val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()
    import com.wavesplatform.state2.diffs._
    val trans = ExchangeTransaction.create(matcher, o1, o2, price, matchedAmount,
      buyFee, sellFee, (buyFee + sellFee) / 2, expiration - 100).explicitGet()

    (trans, matcher)
  }

  val exchangeGenerator: Gen[(ExchangeTransaction, PrivateKeyAccount)] = for {
    sender1: PrivateKeyAccount <- accountGen
    sender2: PrivateKeyAccount <- accountGen
    assetPair <- assetPairGen
    r <- exchangeGeneratorP(sender1, sender2, assetPair.amountAsset, assetPair.priceAsset)
  } yield r

  implicit val orderMatchArb: Arbitrary[(ExchangeTransaction, PrivateKeyAccount)] = Arbitrary {
    exchangeGenerator
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

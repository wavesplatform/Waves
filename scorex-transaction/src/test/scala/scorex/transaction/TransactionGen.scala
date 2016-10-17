package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Matchers._
import org.scalatest.enablers.Containing
import org.scalatest.matchers.{BeMatcher, MatchResult}
import scorex.account.PrivateKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderMatch, Validation}
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.utils.NTP

trait TransactionGen {

  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val bytes64gen: Gen[Array[Byte]] = Gen.listOfN(64, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => new PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  val maxWavesAnountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  val assetPairGen: Gen[AssetPair] = for {
    a: AssetId <- bytes32gen
    b: AssetId <- bytes32gen
  } yield AssetPair(a, b)

  val paymentGenerator: Gen[PaymentTransaction] = for {
    amount: Long <- Gen.choose(0, Long.MaxValue)
    fee: Long <- positiveLongGen
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
  } yield PaymentTransaction(sender, recipient, amount, fee, timestamp)

  val transferGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, Long.MaxValue)
    feeAmount: Long <- positiveLongGen
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    feeAssetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
    recipient: PrivateKeyAccount <- accountGen
  } yield TransferTransaction.create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment)

  val maxTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())

  val orderGenerator: Gen[Order] = for {
    sender: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    spendAssetID: Array[Byte] <- bytes32gen
    receiveAssetID: Array[Byte] <- bytes32gen
    price: Long <- positiveLongGen
    amount: Long <- positiveLongGen
    maxtTime: Long <- maxTimeGen
    matcherFee: Long <- positiveLongGen
  } yield Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxtTime, matcherFee)

  val issueReissueGenerator: Gen[(IssueTransaction, IssueTransaction, ReissueTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    assetName <- genBoundedBytes(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedBytes(0, IssueTransaction.MaxDescriptionLength)
    quantity <- positiveLongGen
    decimals <- Gen.choose(0: Byte, 8: Byte)
    reissuable <- Arbitrary.arbitrary[Boolean]
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val issue = IssueTransaction.create(sender, None, assetName, description, quantity, decimals, reissuable, fee, timestamp)
    val issue2 = IssueTransaction.create(sender, Some(issue.assetId), assetName, description, quantity, decimals,
      reissuable, fee, Math.max(timestamp, 1476459220001L))
    val reissue = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable, fee, timestamp)
    (issue, issue2, reissue)
  }


  val issueGenerator: Gen[IssueTransaction] = issueReissueGenerator.map(_._1)
  val reissueGenerator: Gen[ReissueTransaction] = issueReissueGenerator.map(_._3)

  val invalidOrderGenerator: Gen[Order] = for {
    sender: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    spendAssetID: Array[Byte] <- bytes32gen
    receiveAssetID: Array[Byte] <- bytes32gen
    price: Long <- Arbitrary.arbitrary[Long]
    amount: Long <- Arbitrary.arbitrary[Long]
    maxtTime: Long <- Arbitrary.arbitrary[Long]
    matcherFee: Long <- Arbitrary.arbitrary[Long]
  } yield Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxtTime, matcherFee)

  val orderMatchGenerator: Gen[(OrderMatch, PrivateKeyAccount)] = for {
    sender1: PrivateKeyAccount <- accountGen
    sender2: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    assetPair <- assetPairGen
    spendAssetID: Array[Byte] <- bytes32gen
    receiveAssetID: Array[Byte] <- bytes32gen
    price: Long <- maxWavesAnountGen
    amount1: Long <- maxWavesAnountGen
    amount2: Long <- maxWavesAnountGen
    matchedAmount: Long <- Gen.choose(Math.min(amount1, amount2)/2, Math.min(amount1, amount2))
    maxtTime: Long <- maxTimeGen
    matcherFee: Long <- maxWavesAnountGen
  } yield {
    val o1 = Order.buy(sender1, matcher, assetPair, price, amount1, maxtTime, matcherFee)
    val o2 = Order.sell(sender2, matcher, assetPair, price, amount2, maxtTime, matcherFee)
    val buyFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount1)).longValue()
    val sellFee = (BigInt(matcherFee) * BigInt(matchedAmount) / BigInt(amount2)).longValue()
    val unsigned = OrderMatch(o1, o2, price, matchedAmount,
      buyFee, sellFee, (buyFee + sellFee) / 2, maxtTime - 100, Array())
    val sig = EllipticCurveImpl.sign(matcher, unsigned.toSign)
    (unsigned.copy(signature = sig), matcher)
  }

  implicit val orderMatchArb: Arbitrary[(OrderMatch, PrivateKeyAccount)] = Arbitrary { orderMatchGenerator }
  implicit val privateKeyAccArb: Arbitrary[PrivateKeyAccount] = Arbitrary { accountGen }

  def validOrderMatch = orderMatchGenerator.sample.get

  /**
    * Implicit to support <code>Containing</code> nature of <code>Validation</code>.
    */
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
  val invalid = not (valid)

}

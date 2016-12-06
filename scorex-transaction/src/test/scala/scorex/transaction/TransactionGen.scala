package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import scorex.account.PrivateKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.assets.exchange.{Order, OrderMatch}
import scorex.transaction.assets.{DeleteTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.utils.NTP

trait TransactionGen {

  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val bytes64gen: Gen[Array[Byte]] = Gen.listOfN(64, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genBoundedBytes(minSize: Int, maxSize: Int): Gen[Array[Byte]] = {
    Gen.choose(minSize, maxSize) flatMap { sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]).map(_.toArray) }
  }

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => new PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue / 3)
  val smallFeeGen: Gen[Long] = Gen.choose(1, 100000000)

  val paymentGenerator: Gen[PaymentTransaction] = for {
    amount: Long <- Gen.choose(0, Long.MaxValue)
    fee: Long <- positiveLongGen
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    recipient: PrivateKeyAccount <- accountGen
  } yield PaymentTransaction(sender, recipient, amount, fee, timestamp)

  val selfPaymentGenerator: Gen[PaymentTransaction] = for {
    account: PrivateKeyAccount <- accountGen
    amount: Long <- Gen.choose(0, Long.MaxValue)
    fee: Long <- smallFeeGen
    timestamp: Long <- positiveLongGen
  } yield PaymentTransaction(account, account, amount, fee, timestamp)

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

  val selfTransferGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, Long.MaxValue)
    feeAmount: Long <- smallFeeGen
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    feeAssetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- positiveLongGen
    account: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
  } yield TransferTransaction.create(assetId, account, account, amount, timestamp, feeAssetId, feeAmount, attachment)

  val selfTransferWithWavesFeeGenerator: Gen[TransferTransaction] = for {
    amount: Long <- Gen.choose(0, Long.MaxValue)
    feeAmount: Long <- smallFeeGen
    assetId: Option[Array[Byte]] <- Gen.option(bytes32gen)
    timestamp: Long <- positiveLongGen
    account: PrivateKeyAccount <- accountGen
    attachment: Array[Byte] <- genBoundedBytes(0, TransferTransaction.MaxAttachmentSize)
  } yield TransferTransaction.create(assetId, account, account, amount, timestamp, None, feeAmount, attachment)

  val orderGenerator: Gen[Order] = for {
    sender: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    spendAssetID: Array[Byte] <- bytes32gen
    receiveAssetID: Array[Byte] <- bytes32gen
    price: Long <- positiveLongGen
    amount: Long <- positiveLongGen
    maxtTime: Long <- Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
    matcherFee: Long <- positiveLongGen
  } yield Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxtTime, matcherFee)

  val issueReissueGenerator: Gen[(IssueTransaction, IssueTransaction, ReissueTransaction, DeleteTransaction)] = for {
    sender: PrivateKeyAccount <- accountGen
    assetName <- genBoundedBytes(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
    description <- genBoundedBytes(0, IssueTransaction.MaxDescriptionLength)
    quantity <- positiveLongGen
    deleteAmount <- Gen.choose(0L, quantity)
    decimals <- Gen.choose(0: Byte, 8: Byte)
    reissuable <- Arbitrary.arbitrary[Boolean]
    reissuable2 <- Arbitrary.arbitrary[Boolean]
    fee <- Gen.choose(1L, 2000000L)
    iFee <- Gen.choose(IssueTransaction.MinFee, 2 * IssueTransaction.MinFee)
    timestamp <- positiveLongGen
  } yield {
    val issue = IssueTransaction.create(sender, assetName, description, quantity, decimals, reissuable, iFee, timestamp)
    val issue2 = IssueTransaction.create(sender, assetName, description, quantity, decimals,
      reissuable, iFee, Math.max(timestamp, 1476459220001L))
    val reissue = ReissueTransaction.create(sender, issue.assetId, quantity, reissuable2, fee, timestamp)
    val delete = DeleteTransaction.create(sender, issue.assetId, deleteAmount, fee, timestamp)
    (issue, issue2, reissue, delete)
  }


  val issueGenerator: Gen[IssueTransaction] = issueReissueGenerator.map(_._1)
  val reissueGenerator: Gen[ReissueTransaction] = issueReissueGenerator.map(_._3)
  val deleteGenerator: Gen[DeleteTransaction] = issueReissueGenerator.map(_._4)

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

  val orderMatchGenerator: Gen[OrderMatch] = for {
    sender1: PrivateKeyAccount <- accountGen
    sender2: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    spendAssetID: Array[Byte] <- bytes32gen
    receiveAssetID: Array[Byte] <- bytes32gen
    price: Long <- positiveLongGen
    amount1: Long <- positiveLongGen
    amount2: Long <- positiveLongGen
    matchedAmount: Long <- Gen.choose(1L, Math.min(amount1, amount2))
    maxtTime: Long <- Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
    timestamp: Long <- positiveLongGen
    matcherFee: Long <- positiveLongGen
    fee: Long <- positiveLongGen
  } yield {
    val o1 = Order(sender1, matcher, spendAssetID, receiveAssetID, price, amount1, maxtTime, matcherFee)
    val o2 = Order(sender2, matcher, receiveAssetID, spendAssetID, price, amount2, maxtTime, matcherFee)
    val unsigned = OrderMatch(o1, o2, price, matchedAmount, matcherFee * 2, fee, timestamp, Array())
    val sig = EllipticCurveImpl.sign(matcher, unsigned.toSign)
    OrderMatch(o1, o2, price, matchedAmount, matcherFee * 2, fee, timestamp, sig)
  }


}

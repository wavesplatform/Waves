package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import scorex.account.PrivateKeyAccount
import scorex.transaction.exchange.Order

trait TransactionGen {

  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val bytes64gen: Gen[Array[Byte]] = Gen.listOfN(64, Arbitrary.arbitrary[Byte]).map(_.toArray)

  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => new PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  val paymentGenerator: Gen[PaymentTransaction] = for {
    amount: Long <- Gen.choose(0, Long.MaxValue)
    fee: Long <- positiveLongGen
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    recepient: PrivateKeyAccount <- accountGen
  } yield PaymentTransaction(sender, recepient, amount, fee, timestamp)

  val orderGenerator: Gen[Order] = for {
    sender: PrivateKeyAccount <- accountGen
    matcher: PrivateKeyAccount <- accountGen
    spendAssetID: Array[Byte] <- bytes32gen
    receiveAssetID: Array[Byte] <- bytes32gen
    price: Long <- positiveLongGen
    amount: Long <- positiveLongGen
    maxTime: Long <- positiveLongGen
    matcherFee: Long <- positiveLongGen
  } yield Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee)

}

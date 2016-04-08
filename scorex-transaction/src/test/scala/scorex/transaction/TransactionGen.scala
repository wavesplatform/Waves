package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.utils._

trait TransactionGen {
  val sender = PrivateKeyAccount(randomBytes(32), randomBytes(32), randomBytes(32))

  val paymentGenerator: Gen[PaymentTransaction] = for {
    amount: Long <- Arbitrary.arbitrary[Long]
    fee: Long <- Arbitrary.arbitrary[Long]
    timestamp: Long <- Arbitrary.arbitrary[Long]
  } yield PaymentTransaction(sender, new Account(Account.fromPublicKey(randomBytes(32))), amount, fee, timestamp, randomBytes(64))


}

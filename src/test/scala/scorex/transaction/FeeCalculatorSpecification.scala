package scorex.transaction

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.FeesSettings
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}


class FeeCalculatorSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers with TransactionGen {


  private val configString =
    """waves {
      |  fees {
      |    payment {
      |      WAVES = 100000
      |    }
      |    issue {
      |      WAVES = 100000000
      |    }
      |    transfer {
      |      WAVES = 100000
      |      "JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN" = 2
      |    }
      |    reissue {
      |      WAVES = 200000
      |    }
      |    burn {
      |      WAVES = 300000
      |    }
      |    lease {
      |      WAVES = 400000
      |    }
      |    lease-cancel {
      |      WAVES = 500000
      |    }
      |  }
      |}""".stripMargin

  private val config = ConfigFactory.parseString(configString)

  private val mySettings = FeesSettings.fromConfig(config)

  private val WhitelistedAsset = Base58.decode("JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN").get

  property("Transfer transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(transferGenerator) { tx: TransferTransaction =>
      if (tx.feeAssetId.isEmpty) {
        feeCalc.enoughFee(tx) shouldBe (tx.fee >= 100000)
      } else {
        feeCalc.enoughFee(tx) shouldBe false
      }
    }
  }

  property("Transfer transaction with fee in asset") {
    val feeCalculator = new FeeCalculator(mySettings)
    val sender = new PrivateKeyAccount(Array.emptyByteArray)
    val recipient = new Account("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8")
    val tx1: TransferTransaction = TransferTransaction.create(Some(WhitelistedAsset), sender, recipient, 1000000, 100000000,
      Some(WhitelistedAsset), 2, Array.emptyByteArray).right.get
    val tx2: TransferTransaction = TransferTransaction.create(Some(WhitelistedAsset), sender, recipient, 1000000, 100000000,
      Some(WhitelistedAsset), 1, Array.emptyByteArray).right.get

    feeCalculator.enoughFee(tx1) shouldBe true
    feeCalculator.enoughFee(tx2) shouldBe false
  }

  property("Payment transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(paymentGenerator) { tx: PaymentTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 1000000)
    }
  }

  property("Issue transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(issueGenerator) { tx: IssueTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 100000000)
    }
  }

  property("Reissue transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(reissueGenerator) { tx: ReissueTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 200000)
    }
  }

  property("Burn transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(burnGenerator) { tx: BurnTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 300000)
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(leaseGenerator) { tx: LeaseTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 400000)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(leaseCancelGenerator) { tx: LeaseCancelTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 500000)
    }
  }
}

package scorex.transaction

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.FeesSettings
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.transaction.assets._


class FeeCalculatorSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers with TransactionGen {


  private val configString =
    """waves {
      |  fees = [
      |    {transaction-type: 2, asset: WAVES, fee: 100000}
      |    {transaction-type: 3, asset: WAVES, fee: 100000000}
      |    {transaction-type: 4, asset: WAVES, fee: 100000}
      |    {transaction-type: 4, asset: "JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN", fee: 1002}
      |    {transaction-type: 5, asset: WAVES, fee: 200000}
      |    {transaction-type: 6, asset: WAVES, fee: 300000}
      |  ]
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
}

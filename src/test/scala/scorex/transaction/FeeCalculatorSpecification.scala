package scorex.transaction

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.FeesSettings
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import scorex.crypto.encode.Base58
import scorex.transaction.assets._


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
      |      "JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN" = 1002
      |    }
      |    reissue {
      |      WAVES = 200000
      |    }
      |    burn {
      |      WAVES = 300000
      |    }
      |  }
      |}""".stripMargin

  private val config = ConfigFactory.parseString(configString)

  private val mySettings = FeesSettings.fromConfig(config)

  private val WhitelistedAsset = Base58.decode("JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN").get

  implicit class ConditionalAssert(v: Either[_, _]) {

    def shouldBeRightIf(cond: Boolean): Assertion = {
      if (cond) {
        v shouldBe an[Right[_, _]]
      } else {
        v shouldBe an[Left[_, _]]
      }
    }
  }

  property("Transfer transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(transferGenerator) { tx: TransferTransaction =>
      if (tx.feeAssetId.isEmpty) {
        feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000)
      } else {
        feeCalc.enoughFee(tx) shouldBe an[Left[_,_]]
      }
    }
  }

  property("Payment transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(paymentGenerator) { tx: PaymentTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 1000000)
    }
  }

  property("Issue transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(issueGenerator) { tx: IssueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000000)
    }
  }

  property("Reissue transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(reissueGenerator) { tx: ReissueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 200000)
    }
  }

  property("Burn transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(burnGenerator) { tx: BurnTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 300000)
    }
  }
}

package scorex.transaction

import java.net.InetSocketAddress

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction.assets._


class FeeCalculatorSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers with TransactionGen {

  val WhitelistedAsset = Base58.decode("JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN").get

  property("Transfer transaction ") {
    val feeCalc = new FeeCalculator(MySettings)
    forAll(transferGenerator) { tx: TransferTransaction =>
      if (tx.feeAssetId.isEmpty) {
        feeCalc.enoughFee(tx) shouldBe (tx.fee >= 100000)
      } else {
        feeCalc.enoughFee(tx) shouldBe false
      }
    }
  }

  property("Payment transaction ") {
    val feeCalc = new FeeCalculator(MySettings)
    forAll(paymentGenerator) { tx: PaymentTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 1000000)
    }
  }

  property("Issue transaction ") {
    val feeCalc = new FeeCalculator(MySettings)
    forAll(issueGenerator) { tx: IssueTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 100000000)
    }
  }

  property("Reissue transaction ") {
    val feeCalc = new FeeCalculator(MySettings)
    forAll(reissueGenerator) { tx: ReissueTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 200000)
    }
  }

  property("Burn transaction ") {
    val feeCalc = new FeeCalculator(MySettings)
    forAll(burnGenerator) { tx: BurnTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 300000)
    }
  }

  private val str =
    """{
      |"feeMap": {
      |  "2": {
      |    "Waves": 100000
      |  },
      |  "3": {
      |    "Waves": 100000000
      |  },
      |  "4": {
      |    "Waves": 100000,
      |    "JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN": 1002
      |  },
      |  "5": {
      |    "Waves": 200000
      |  },
      |  "6": {
      |    "Waves": 300000
      |  }
      |}}""".stripMargin
  val feeMapJson: JsObject = Json.parse(str).as[JsObject]


  object MySettings extends Settings {
    override lazy val settingsJSON: JsObject = feeMapJson
    override lazy val dataDirOpt: Option[String] = None
    override lazy val knownPeers = Seq.empty[InetSocketAddress]
  }

}

package scorex.transaction

import java.net.InetSocketAddress

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction, TransferTransaction}


class FeeCalculatorSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
with Matchers with TransactionGen {

  val WhitelistedAsset = Base58.decode("JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN").get

  property("Transfer transaction ") {
    val feeCalc = new FeeCalculator(MySettings)
    forAll(transferGenerator) { tx: TransferTransaction =>
      if (tx.feeAsset.isEmpty) {
        feeCalc.enoughFee(tx) shouldBe (tx.fee >= 100000)
      } else {
        feeCalc.enoughFee(tx) shouldBe false
        val tx2: TransferTransaction = tx.feeAsset.map(t => tx.copy(feeAsset = Some(WhitelistedAsset))).getOrElse(tx)
        feeCalc.enoughFee(tx2) shouldBe (tx.fee >= 1002)
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
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 20000000)
    }
  }

  property("Reissue transaction ") {
    val feeCalc = new FeeCalculator(MySettings)
    forAll(reissueGenerator) { tx: ReissueTransaction =>
      feeCalc.enoughFee(tx) shouldBe (tx.fee >= 200000)
    }
  }

  private val str =
    """{
      |"feeMap": {
      |  "2": {
      |    "Waves": 100000
      |  },
      |  "3": {
      |    "Waves": 10000000
      |  },
      |  "4": {
      |    "Waves": 100000,
      |    "JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN": 1002
      |  },
      |  "5": {
      |    "Waves": 100000
      |  }
      |}}""".stripMargin
  val feeMapJson: JsObject = Json.parse(str).as[JsObject]


  object MySettings extends TransactionSettings with Settings {
    override lazy val settingsJSON: JsObject = feeMapJson
    override lazy val dataDirOpt: Option[String] = None
    override lazy val knownPeers = Seq.empty[InetSocketAddress]
  }
}

package scorex.transaction

import com.typesafe.config.ConfigFactory
import com.wavesplatform.TransactionGen
import com.wavesplatform.settings.FeesSettings
import com.wavesplatform.state.ByteStr
import com.wavesplatform.state._
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.script.Script

class FeeCalculatorSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen with MockFactory {

  private val configString =
    """waves {
      |  fees {
      |    smart-account.extra-fee = 10
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
      |    create-alias {
      |      WAVES = 600000
      |    }
      |    data {
      |      WAVES = 100000
      |    }
      |  }
      |}""".stripMargin

  private val config = ConfigFactory.parseString(configString)

  private val mySettings = FeesSettings.fromConfig(config)

  private val WhitelistedAsset = ByteStr.decodeBase58("JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN").get

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
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(transferGen) { tx: TransferTransaction =>
      if (tx.feeAssetId.isEmpty) {
        feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000)
      } else {
        feeCalc.enoughFee(tx) shouldBe an[Left[_, _]]
      }
    }
  }

  property("Transfer transaction with fee in asset") {
    val feeCalculator = new FeeCalculator(mySettings, noScriptBlockchain)
    val sender        = PrivateKeyAccount(Array.emptyByteArray)
    val recipient     = Address.fromString("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8").right.get
    val tx1: TransferTransaction = TransferTransaction
      .create(Some(WhitelistedAsset), sender, recipient, 1000000, 100000000, Some(WhitelistedAsset), 12, Array.emptyByteArray)
      .right
      .get
    val tx2: TransferTransaction = TransferTransaction
      .create(Some(WhitelistedAsset), sender, recipient, 1000000, 100000000, Some(WhitelistedAsset), 1, Array.emptyByteArray)
      .right
      .get

    feeCalculator.enoughFee(tx1) shouldBe a[Right[_, _]]
    feeCalculator.enoughFee(tx2) shouldBe a[Left[_, _]]
  }

  property("Payment transaction ") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(paymentGen) { tx: PaymentTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000)
    }
  }

  property("Issue transaction ") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(issueGen) { tx: IssueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000000)
    }
  }

  property("Reissue transaction ") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(reissueGen) { tx: ReissueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 200000)
    }
  }

  property("Burn transaction ") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(burnGen) { tx: BurnTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 300000)
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(leaseGen) { tx: LeaseTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 400000)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 500000)
    }
  }

  property("Create alias transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 600000)
    }
  }

  property("Data transaction") {
    val feeCalc = new FeeCalculator(mySettings, noScriptBlockchain)
    forAll(dataTransactionGen) { tx =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= Math.ceil(tx.bytes().length / 1024.0) * 100000)
    }
  }

  private def createBlockchain(accountScript: Address => Option[Script]): Blockchain = {
    val r = stub[Blockchain]
    (r.accountScript _).when(*).onCall((addr: Address) => accountScript(addr)).anyNumberOfTimes()
    r
  }

  private def noScriptBlockchain: Blockchain = createBlockchain(_ => None)
}

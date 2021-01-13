package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.Assertion

import scala.concurrent.duration._

class RideV4ActivationSuite extends BaseTransactionSuite {

  import RideV4ActivationSuite._

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5, activationHeight - 1)))
      .buildNonConflicting()

  private def smartAccV4 = firstKeyPair
  private def callerAcc  = secondKeyPair
  private def smartAccV3 = thirdKeyPair

  private var asset: Asset = _

  private val dAppV4 =
    """{-# STDLIB_VERSION 4 #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |
      |@Callable(i)
      |func default () = [BooleanEntry("0", true)]
      |
      |
      |
      |@Callable(i)
      |func payBack () = [ScriptTransfer(i.caller, i.payments[0].amount, value(i.payments[0].assetId))]
      |
      |""".stripMargin

  private val dAppV3 =
    """{-# STDLIB_VERSION 3 #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |@Callable(i)
      |func default() = WriteSet([DataEntry("0", true)])
      |
      |@Callable(i)
      |func payBack() = {
      |    let pmt = value(i.payment)
      |    TransferSet([ ScriptTransfer(i.caller, pmt.amount, value(pmt.assetId)) ])
      |}""".stripMargin
  private val accountV4 =
    """{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |(tx.sender == this)""".stripMargin
  private val accountV3 =
    """{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |(tx.sender == this)""".stripMargin
  private val assetV4 =
    """
      |{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |this.quantity > 0
      |""".stripMargin
  private val assetV3 =
    """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ASSET #-}
      |this.quantity > 0
      |""".stripMargin

  test("can't set V4 contracts before the feature activation") {
    def assertFeatureNotActivated[R](f: => R): Assertion = assertApiError(f) { e =>
      e.statusCode shouldBe 400
      e.id shouldBe StateCheckFailed.Id
      e.message should include("Ride V4, VRF, Protobuf, Failed transactions feature has not been activated")
    }

    assertFeatureNotActivated(miner.setScript(smartAccV4, Some(dAppV4.compiled)))
    assertFeatureNotActivated(miner.setScript(smartAccV4, Some(accountV4.compiled)))
    assertFeatureNotActivated(
      miner.issue(
        smartAccV4,
        "Asset",
        "",
        1,
        0,
        script = Some(assetV4.compiled)
      )
    )

    val assetId = miner
      .issue(smartAccV4, "Asset", "", 1, 0, script = Some(assetV3.compiled))
      .id
    assertFeatureNotActivated(
      miner.setAssetScript(
        assetId,
        smartAccV4,
        script = Some(accountV4.compiled),
        fee = setAssetScriptFee + smartFee
      )
    )
  }

  test("can compile via API before the feature activation") {
    miner.scriptCompile(dAppV4).script shouldBe dAppV4.compiled
    miner.scriptCompile(accountV4).script shouldBe accountV4.compiled
    miner.scriptCompile(assetV4).script shouldBe assetV4.compiled
  }

  test("can decompile via API before the feature activation") {
    miner.scriptDecompile(dAppV4.compiled).script shouldBe dAppV4
    miner.scriptDecompile(accountV4.compiled).script shouldBe accountV4
    miner.scriptDecompile(assetV4.compiled).script shouldBe
      """{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |(this.quantity > 0)""".stripMargin
  }

  test("declines reduced fee for reissue transaction before activation") {
    val issuedAssetId =
      miner.issue(callerAcc, "name5", "description5", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true).id

    assertApiError(miner.reissue(callerAcc, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueReducedFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(s"Fee for ReissueTransaction ($reissueReducedFee in WAVES) does not exceed minimal value of $reissueFee WAVES.")
    }
  }

  test("can't attach unavailable payment if V3 DApp returns its enough amount") {
    val amount = 20
    assertApiError(
      miner.invokeScript(callerAcc, smartAccV3.toAddress.toString, Some("payBack"), payment = Seq(Payment(amount, asset)), waitForTx = true)
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe
        "State check failed. Reason: " +
          "Attempt to transfer unavailable funds: " +
          "Transaction application leads to negative asset " +
          s"'$asset' balance to " +
          "(at least) temporary negative state, " +
          s"current balance is 0, spends equals -$amount, result is -$amount"
    }
  }

  test("wait for the feature activation") {
    miner.waitForHeight(activationHeight, 5.minutes)
  }

  test("can set asset script V4 after the function activation") {
    val assetId = miner
      .issue(
        smartAccV4,
        "Test",
        "",
        1000,
        0,
        script = Some(assetV4.compiled),
        waitForTx = true
      )
      .id

    miner.setAssetScript(assetId, smartAccV4, setAssetScriptFee + smartFee, Some(assetV4.compiled), waitForTx = true)
  }

  test("can set and invoke account script V4 after the feature activation") {
    miner.setScript(smartAccV4, Some(accountV4.compiled), waitForTx = true)
    miner.setScript(smartAccV4, Some(dAppV4.compiled), fee = setScriptFee + smartFee, waitForTx = true)

    miner.invokeScript(callerAcc, smartAccV4.toAddress.toString, waitForTx = true)
  }

  test("can invoke V4 contract from V3 scripted account with 0 or 1 payments") {
    miner.setScript(smartAccV3, Some(accountV3.compiled), fee = setScriptFee + smartFee, waitForTx = true)

    miner.invokeScript(smartAccV3, smartAccV4.toAddress.toString, fee = smartMinFee + smartFee, waitForTx = true)._1.id
    miner
      .invokeScript(
        smartAccV3,
        smartAccV4.toAddress.toString,
        payment = Seq(Payment(1, Waves)),
        fee = smartMinFee + smartFee,
        waitForTx = true
      )
      ._1
      .id
  }

  test("can't invoke V4 contract from V3 scripted account with 2 payments") {
    miner.setScript(smartAccV3, Some(accountV3.compiled), fee = setScriptFee + smartFee, waitForTx = true)

    assertApiError(
      miner
        .invokeScript(
          smartAccV3,
          smartAccV4.toAddress.toString,
          payment = Seq(Payment(1, Waves), Payment(1, Waves)),
          fee = smartMinFee + smartFee,
          waitForTx = true
        )
        ._1
        .id
    ) { error =>
      error.statusCode shouldBe 400
      error.id shouldBe ScriptExecutionError.Id
      error.message should include("Invoker script version 3 < 4 doesn't support multiple payment attachment")
    }
  }

  test("can't invoke V3 DApp with multiple payments") {
    miner.setScript(smartAccV3, Some(dAppV3.compiled), fee = setScriptFee + smartFee, waitForTx = true)

    assertApiError(
      miner.invokeScript(callerAcc, smartAccV3.toAddress.toString, payment = Seq(Payment(1, Waves), Payment(1, Waves)), waitForTx = true)
    ) { error =>
      error.statusCode shouldBe 400
      error.message should include("DApp version 3 < 4 doesn't support multiple payment attachment")
      error.id shouldBe StateCheckFailed.Id
    }
  }

  test("can't use V4 features in V3 even after activation") {
    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ BooleanEntry("0", true) ])"""))) { error =>
      error.message should include("Can't find a function 'BooleanEntry'(String, Boolean)")
    }
    assertApiError(miner.scriptCompile(asDappV3(s"""[ DataEntry("0", true) ]"""))) { error =>
      error.message should include("CallableFunction needs to return ScriptResult|TransferSet|WriteSet")
    }
    assertApiError(miner.scriptCompile(asDappV3(s"""[ BooleanEntry("0", true) ]"""))) { error =>
      error.message should include("Can't find a function 'BooleanEntry'(String, Boolean)")
    }

    assertApiError(miner.scriptCompile(asAssetV3(s"""let x = BooleanEntry("0", true); true"""))) { error =>
      error.message should include("Can't find a function 'BooleanEntry'(String, Boolean)")
    }

    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ DataEntry("x", inv.payments[0].amount) ])"""))) { error =>
      error.message should include("Undefined field `payments` of variable of type `Invocation`")
    }
    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ DataEntry("x", inv.payment[0].amount) ])"""))) { error =>
      error.message should include("Non-matching types: expected: List[T], actual: AttachedPayment|Unit")
    }
    assertApiError(miner.scriptCompile(asAssetV3(s"""match tx {
           |  case is: InvokeScriptTransaction => is.payments[0].amount > 0
           |  case _ => true }""".stripMargin))) { error =>
      error.message should include("Undefined field `payments` of variable of type `InvokeScriptTransaction`")
    }
    assertApiError(miner.scriptCompile(asAssetV3(s"""match tx {
           |  case is: InvokeScriptTransaction => is.payment[0].amount > 0
           |  case _ => true }""".stripMargin))) { error =>
      error.message should include("Non-matching types: expected: List[T], actual: AttachedPayment|Unit")
    }

    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ DataEntry("x", median([1, 2, 3])) ])"""))) { error =>
      error.message should include("Can't find a function 'median'(List[Int])")
    }
    assertApiError(miner.scriptCompile(asAssetV3(s"""median([1, 2, 3]) > 0"""))) { error =>
      error.message should include("Can't find a function 'median'(List[Int])")
    }

    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ DataEntry("x", contains("abc", "b")) ])"""))) { error =>
      error.message should include("Can't find a function 'contains'(String, String)")
    }
    assertApiError(miner.scriptCompile(asAssetV3(s"""contains("abc", "b")"""))) { error =>
      error.message should include("Can't find a function 'contains'(String, String)")
    }

    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ DataEntry("x", valueOrElse(parseInt("1"), "error")) ])"""))) { error =>
      error.message should include("Can't find a function 'valueOrElse'(Int|Unit, String)")
    }
    assertApiError(miner.scriptCompile(asAssetV3(s"""valueOrElse(parseInt("1"), "error") > 0"""))) { error =>
      error.message should include("Can't find a function 'valueOrElse'(Int|Unit, String)")
    }

    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ DataEntry("x", true) ] ++ [ DataEntry("y", false) ])"""))) { error =>
      error.message should include("Can't find a function '++'(List[DataEntry], List[DataEntry])")
    }
    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([ DataEntry("x", true) ] :+ DataEntry("y", false) )"""))) { error =>
      error.message should include("Can't find a function ':+'(List[DataEntry], DataEntry)")
    }

    assertApiError(miner.scriptCompile(asDappV3(s"""WriteSet([])}
           |@Callable(inv)
           |func withArg(a: List[Int]) = { WriteSet([ DataEntry("a", a[0]) ])""".stripMargin))) { error =>
      error.message should include("Unexpected callable func arg type: List[Int] ")
    }
  }

  test("can't use deprecated V3 features in V4 script") {
    assertApiError(miner.scriptCompile(asDappV4(s"""WriteSet([])""".stripMargin))) { error =>
      error.message should include("Can't find a function 'WriteSet'(List[Nothing])")
    }
    assertApiError(miner.scriptCompile(asAssetV4(s"""let x = WriteSet([]); true""".stripMargin))) { error =>
      error.message should include("Can't find a function 'WriteSet'(List[Nothing])")
    }
    assertApiError(miner.scriptCompile(asDappV4(s"""TransferSet([])""".stripMargin))) { error =>
      error.message should include("Can't find a function 'TransferSet'(List[Nothing])")
    }
    assertApiError(miner.scriptCompile(asAssetV4(s"""let x = TransferSet([]); true""".stripMargin))) { error =>
      error.message should include("Can't find a function 'TransferSet'(List[Nothing])")
    }
    assertApiError(miner.scriptCompile(asDappV4(s"""[ DataEntry("x", true) ]""".stripMargin))) { error =>
      error.message should include("Can't find a function 'DataEntry'(String, Boolean)")
    }
    assertApiError(miner.scriptCompile(asAssetV4(s"""let x = DataEntry("x", true); true""".stripMargin))) { error =>
      error.message should include("Can't find a function 'DataEntry'(String, Boolean)")
    }

    assertApiError(miner.scriptCompile(asDappV4(s"""[ IntegerEntry("x", inv.payment.extract().amount) ]"""))) { error =>
      error.message should include("Undefined field `payment` of variable of type `Invocation`")
    }
    assertApiError(miner.scriptCompile(asAssetV4(s"""match tx {
           |  case is: InvokeScriptTransaction =>
           |    let x = is.payment.extract().amount
           |    true
           |  case _ => true }""".stripMargin))) { error =>
      error.message should include("Undefined field `payment` of variable of type `InvokeScriptTransaction`")
    }
  }

  test("accepts reduced fee for reissue transaction after activation") {
    val issuedAssetId =
      miner.issue(callerAcc, "name5", "description5", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true).id

    val reissueTxId = miner.reissue(callerAcc, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueReducedFee).id
    nodes.waitForHeightAriseAndTxPresent(reissueTxId)
  }
  test("can't attach unavailable payment even if V4 DApp returns its enough amount") {
    val balance = miner.wavesBalance(callerAcc.toAddress.toString)
    assertApiError(
      miner.invokeScript(callerAcc, smartAccV4.toAddress.toString, Some("payBack"), payment = Seq(Payment(40, asset)))
    ) { error =>
      error.message should include("Transaction application leads to negative asset")
      error.id shouldBe StateCheckFailed.Id
      error.statusCode shouldBe 400
    }

    assertApiError(
      miner.invokeScript(callerAcc, smartAccV4.toAddress.toString, Some("payBack"), payment = Seq(Payment(balance + 1, Waves)))
    ) { error =>
      error.message should include("Transaction application leads to negative waves balance")
      error.id shouldBe StateCheckFailed.Id
      error.statusCode shouldBe 400
    }
  }

  test("still can't attach unavailable payment if V3 DApp returns its enough amount") {
    val balance = miner.wavesBalance(callerAcc.toAddress.toString)
    assertApiError(
      miner.invokeScript(callerAcc, smartAccV3.toAddress.toString, Some("payBack"), payment = Seq(Payment(40, asset)), waitForTx = true)
    )(_.statusCode shouldBe 400)

    assertApiError(
      miner.invokeScript(callerAcc, smartAccV3.toAddress.toString, Some("payBack"), payment = Seq(Payment(balance, Waves)), waitForTx = true)
    )(_.statusCode shouldBe 400)
  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val assetId = miner.issue(smartAccV3, quantity = 1000, waitForTx = true).id
    asset = IssuedAsset(ByteStr.decodeBase58(assetId).get)
    miner.setScript(smartAccV3, Some(dAppV3.compiled), waitForTx = true)
  }
}

object RideV4ActivationSuite {
  private val estimator = ScriptEstimatorV2
  val activationHeight  = 9

  def asAssetV3(body: String): String = {
    s"""{-# STDLIB_VERSION 3 #-}
       |{-# SCRIPT_TYPE ASSET #-}
       |{-# CONTENT_TYPE EXPRESSION #-}
       |$body
       |""".stripMargin
  }

  def asAssetV4(body: String): String = {
    s"""{-# STDLIB_VERSION 4 #-}
       |{-# SCRIPT_TYPE ASSET #-}
       |{-# CONTENT_TYPE EXPRESSION #-}
       |$body
       |""".stripMargin
  }

  def asDappV3(body: String): String = {
    s"""{-# STDLIB_VERSION 3 #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# CONTENT_TYPE DAPP #-}
       |@Callable(inv)
       |func default() = {
       |$body
       |}
       |""".stripMargin
  }

  def asDappV4(body: String): String = {
    s"""{-# STDLIB_VERSION 4 #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# CONTENT_TYPE DAPP #-}
       |@Callable(inv)
       |func default() = {
       |$body
       |}
       |""".stripMargin
  }

  implicit class CompiledFromString(script: String) {
    def compiled: String = ScriptCompiler.compile(script, estimator).explicitGet()._1.bytes().base64
  }

}

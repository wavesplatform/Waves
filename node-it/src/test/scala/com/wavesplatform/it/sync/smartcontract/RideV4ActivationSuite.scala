package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{Assertion, CancelAfterFailure}

import scala.concurrent.duration._

class RideV4ActivationSuite extends BaseTransactionSuite with CancelAfterFailure {
  import RideV4ActivationSuite._

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((16, activationHeight - 1)))
      .buildNonConflicting()

  private val smartAcc  = pkByAddress(firstAddress).stringRepr
  private val callerAcc = pkByAddress(secondAddress).stringRepr

  private val dAppV4 =
    """{-# STDLIB_VERSION 4 #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |
      |@Callable(i)
      |func default () = [BooleanEntry("0", true)]
      |
      |""".stripMargin
  private val accountV4 =
    """{-# STDLIB_VERSION 4 #-}
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
      e.message should include("Multiple payment attachment for Invoke Script Transaction feature has not been activated")
    }

    assertFeatureNotActivated(sender.setScript(smartAcc, Some(dAppV4.compiled)))
    assertFeatureNotActivated(sender.setScript(smartAcc, Some(accountV4.compiled)))
    assertFeatureNotActivated(sender.issue(
      smartAcc, "Asset", "", 1, 0, script = Some(assetV4.compiled)
    ))

    val assetId = sender
      .issue(smartAcc, "Asset", "", 1, 0, script = Some(assetV3.compiled))
      .id
    assertFeatureNotActivated(sender.setAssetScript(
      assetId, smartAcc, script = Some(accountV4.compiled), fee = setAssetScriptFee + smartFee
    ))
  }

  test("can compile via API before the feature activation") {
    sender.scriptCompile(dAppV4).script shouldBe dAppV4.compiled
    sender.scriptCompile(accountV4).script shouldBe accountV4.compiled
    sender.scriptCompile(assetV4).script shouldBe assetV4.compiled
  }

  test("can decompile via API before the feature activation") {
    sender.scriptDecompile(dAppV4.compiled).script shouldBe dAppV4
    sender.scriptDecompile(accountV4.compiled).script shouldBe accountV4
    sender.scriptDecompile(assetV4.compiled).script shouldBe
      """{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE EXPRESSION #-}
        |(this.quantity > 0)""".stripMargin
  }

  //TODO rejected invoke V3 with multiple payments

  test(s"wait height $activationHeight for the feature activation") {
    sender.waitForHeight(activationHeight, 5.minutes)
  }

  test("can set asset script V4 after the function activation") {
    val assetId = sender.issue(
      smartAcc, "Test", "", 1000, 0, script = Some(assetV4.compiled), waitForTx = true
    ).id

    sender
      .setAssetScript(assetId, smartAcc, setAssetScriptFee + smartFee, Some(assetV4.compiled), waitForTx = true)
      .id
  }

  test("can set account script V4 after the feature activation") {
    sender.setScript(smartAcc, Some(accountV4.compiled), waitForTx = true)
    sender.setScript(smartAcc, Some(dAppV4.compiled), fee = setScriptFee + smartFee, waitForTx = true)

    //TODO rejected invoke V3 with multiple payments
    //TODO accepted invoke V4 with multiple payments
    sender.invokeScript(callerAcc, smartAcc, None, waitForTx = true)._1.id
  }

  test("can't use V4 features in V3 even after activation") {
    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ BooleanEntry("0", true) ])"""))) { error =>
      error.message should include("Can't find a function 'BooleanEntry'(String, Boolean)") }
    assertApiError(sender.scriptCompile(
      asDappV3(s"""[ DataEntry("0", true) ]"""))) { error =>
      error.message should include("CallableFunction needs to return ScriptResult|TransferSet|WriteSet") }
    assertApiError(sender.scriptCompile(
      asDappV3(s"""[ BooleanEntry("0", true) ]"""))) { error =>
      error.message should include("Can't find a function 'BooleanEntry'(String, Boolean)") }

    assertApiError(sender.scriptCompile(
      asAssetV3(s"""let x = BooleanEntry("0", true); true"""))) { error =>
      error.message should include("Can't find a function 'BooleanEntry'(String, Boolean)") }

    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ DataEntry("x", inv.payments[0].amount) ])"""))) { error =>
      error.message should include("Undefined field `payments` of variable of type `Invocation`") }
    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ DataEntry("x", inv.payment[0].amount) ])"""))) { error =>
      error.message should include("Non-matching types: expected: List[T], actual: AttachedPayment|Unit") }
    assertApiError(sender.scriptCompile(
      asAssetV3(
        s"""match tx {
           |  case is: InvokeScriptTransaction => is.payments[0].amount > 0
           |  case _ => true }""".stripMargin))) { error =>
      error.message should include("Undefined field `payments` of variable of type `InvokeScriptTransaction`") }
    assertApiError(sender.scriptCompile(
      asAssetV3(
        s"""match tx {
           |  case is: InvokeScriptTransaction => is.payment[0].amount > 0
           |  case _ => true }""".stripMargin))) { error =>
      error.message should include("Non-matching types: expected: List[T], actual: AttachedPayment|Unit") }

    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ DataEntry("x", median([1, 2, 3])) ])"""))) { error =>
      error.message should include("Can't find a function 'median'(List[Int])") }
    assertApiError(sender.scriptCompile(
      asAssetV3(s"""median([1, 2, 3]) > 0"""))) { error =>
      error.message should include("Can't find a function 'median'(List[Int])") }

    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ DataEntry("x", contains("abc", "b")) ])"""))) { error =>
      error.message should include("Can't find a function 'contains'(String, String)") }
    assertApiError(sender.scriptCompile(
      asAssetV3(s"""contains("abc", "b")"""))) { error =>
      error.message should include("Can't find a function 'contains'(String, String)") }

    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ DataEntry("x", valueOrElse(parseInt("1"), "error")) ])"""))) { error =>
      error.message should include("Can't find a function 'valueOrElse'(Int|Unit, String)") }
    assertApiError(sender.scriptCompile(
      asAssetV3(s"""valueOrElse(parseInt("1"), "error") > 0"""))) { error =>
      error.message should include("Can't find a function 'valueOrElse'(Int|Unit, String)") }

    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ DataEntry("x", true) ] ++ [ DataEntry("y", false) ])"""))) { error =>
      error.message should include("Can't find a function '++'(List[DataEntry], List[DataEntry])") }
    assertApiError(sender.scriptCompile(
      asDappV3(s"""WriteSet([ DataEntry("x", true) ] :+ DataEntry("y", false) )"""))) { error =>
      error.message should include("Can't find a function ':+'(List[DataEntry], DataEntry)") }

    assertApiError(sender.scriptCompile(
      asDappV3(
        s"""WriteSet([])}
           |@Callable(inv)
           |func withArg(a: List[Int]) = { WriteSet([ DataEntry("a", a[0]) ])""".stripMargin))) { error =>
      error.message should include("Annotated function should not have generic parameter types") }
  }

  test("can't use deprecated V3 features in V4 script") {
    assertApiError(sender.scriptCompile(
      asDappV4(s"""WriteSet([])""".stripMargin))) { error =>
      error.message should include("Can't find a function 'WriteSet'(List[Nothing])") }
    assertApiError(sender.scriptCompile(
      asAssetV4(s"""let x = WriteSet([]); true""".stripMargin))) { error =>
      error.message should include("Can't find a function 'WriteSet'(List[Nothing])") }
    assertApiError(sender.scriptCompile(
      asDappV4(s"""TransferSet([])""".stripMargin))) { error =>
      error.message should include("Can't find a function 'TransferSet'(List[Nothing])") }
    assertApiError(sender.scriptCompile(
      asAssetV4(s"""let x = TransferSet([]); true""".stripMargin))) { error =>
      error.message should include("Can't find a function 'TransferSet'(List[Nothing])") }
    assertApiError(sender.scriptCompile(
      asDappV4(s"""[ DataEntry("x", true) ]""".stripMargin))) { error =>
      error.message should include("Can't find a function 'DataEntry'(String, Boolean)") }
    assertApiError(sender.scriptCompile(
      asAssetV4(s"""let x = DataEntry("x", true); true""".stripMargin))) { error =>
      error.message should include("Can't find a function 'DataEntry'(String, Boolean)") }

    assertApiError(sender.scriptCompile(
      asDappV4(s"""[ IntEntry("x", inv.payment.extract().amount) ]"""))) { error =>
      error.message should include("Undefined field `payment` of variable of type `Invocation`") }
    assertApiError(sender.scriptCompile(
      asAssetV4(
        s"""match tx {
           |  case is: InvokeScriptTransaction =>
           |    let x = is.payment.extract().amount
           |    true
           |  case _ => true }""".stripMargin))) { error =>
      error.message should include("Undefined field `payment` of variable of type `InvokeScriptTransaction`") }
  }

}

object RideV4ActivationSuite {
  private val estimator = ScriptEstimatorV2
  val activationHeight = 9

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

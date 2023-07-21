package com.wavesplatform.ride.runner

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.utils.UtilsEvaluator
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.ride.runner.blockchain.ImmutableBlockchain
import com.wavesplatform.ride.runner.environments.{DAppEnvironmentTracker, TrackedDAppEnvironment}
import com.wavesplatform.ride.runner.input.*
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.TxNonNegativeAmount
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.Json

import java.util.concurrent.atomic.AtomicInteger

class ExpectedHeightCallsTestSuite extends BaseTestSuite with HasTestAccounts {
  private val DefaultHeight = Height @@ 3565654

  "Blockchain.height is used in script" - {
    "never" - {
      "plain invoke" in invokeAliceFooTest(
        expectedHeightCalls = 0,
        aliceAddr ->
          """@Callable(inv)
            |func foo() = {
            |  []
            |}""".stripMargin
      )
      "invoke from invoke" in invokeAliceFooTest(
        0,
        aliceAddr ->
          s"""@Callable(inv)
             |func foo() = {
             | strict res = invoke(Address(base58'$bobAddr'), "bar", [], [])
             | ([IntegerEntry("i", 0)], res)
             |}""".stripMargin,
        bobAddr ->
          """@Callable(inv)
            |func bar() = {
            |  ([], 0)
            |}""".stripMargin
      )
    }

    "once" - {
      "plain invoke" in invokeAliceFooTest(
        expectedHeightCalls = 1,
        aliceAddr ->
          """@Callable(inv)
            |func foo() = {
            | ([], height + 1)
            |}""".stripMargin
      )

      "invoke from invoke" in invokeAliceFooTest(
        1,
        aliceAddr ->
          s"""@Callable(inv)
             |func foo() = {
             | strict res = invoke(Address(base58'$bobAddr'), "bar", [], [])
             | ([IntegerEntry("i", 0)], res)
             |}""".stripMargin,
        bobAddr ->
          """@Callable(inv)
            |func bar() = {
            |  ([], height)
            |}""".stripMargin
      )
    }
  }

  private def invokeAliceFooTest(expectedHeightCalls: Int, scriptsSrcs: (Address, String)*): Unit = {
    val input = scriptsSrcs.foldLeft(defaultInput.state) { case (r, (addr, scriptSrc)) =>
      val orig = r.accounts(addr)
      r.copy(accounts =
        r.accounts.updated(
          addr,
          orig.copy(scriptInfo =
            Some(
              RideRunnerScriptInfo(
                script = TestScript.scriptFrom(
                  s"""{-#STDLIB_VERSION 6 #-}
                     |{-#SCRIPT_TYPE ACCOUNT #-}
                     |{-#CONTENT_TYPE DAPP #-}
                     |
                     $scriptSrc""".stripMargin
                )
              )
            )
          )
        )
      )
    }
    val blockchain = new ImmutableBlockchain(DefaultBlockchainSettings, input)
    val counter    = new CountedHeightCallsTracker
    val r = UtilsEvaluator.evaluate(
      blockchain = blockchain,
      dAppAddress = aliceAddr,
      request = Json.obj("expr" -> "foo()"),
      options = UtilsEvaluator.EvaluateOptions(
        evaluateScriptComplexityLimit = Int.MaxValue,
        maxTxErrorLogSize = 0,
        enableTraces = false,
        intAsString = true
      ),
      wrapDAppEnv = underlying => new TrackedDAppEnvironment(underlying, counter)
    )

    withClue(s"$r") {
      (r \ "error").asOpt[Int] shouldBe empty
      counter.heightCalls.get() shouldBe expectedHeightCalls
    }
  }

  private lazy val defaultInput = RideRunnerInput(
    address = scriptedAccAddr,
    request = Json.obj(),
    state = RideRunnerBlockchainState(
      height = DefaultHeight,
      accounts = Map(
        aliceAddr -> RideRunnerAccount(
          regularBalance = Some(TxNonNegativeAmount(10_000_000))
        ),
        bobAddr -> RideRunnerAccount(
          data = Some(Map.empty),
          regularBalance = Some(TxNonNegativeAmount(10_300_000))
        )
      ),
      blocks = Map(
        DefaultHeight -> RideRunnerBlock(
          timestamp = 1663299568885L,
          VRF = Some(ByteStr.decodeBase58("GHC3DQuW9ncm5sNy5u3TVEF4CXu1fsLVHVHYxJzuZr7b").get)
        )
      )
    )
  )

  private class CountedHeightCallsTracker extends DAppEnvironmentTracker {
    val heightCalls = new AtomicInteger(0)

    override def height(): Unit = heightCalls.incrementAndGet()

    override def callScript(dApp: Address): Unit = {}

    override def transactionById(id: Array[Byte]): Unit = kill(s"transactionById(${ByteStr(id)})")

    override def transferTransactionById(id: Array[Byte]): Unit = kill(s"transferTransactionById(${ByteStr(id)})")

    override def transactionHeightById(id: Array[Byte]): Unit = kill(s"transactionHeightById(${ByteStr(id)})")

    override def assetInfoById(id: Array[Byte]): Unit = kill(s"assetInfoById(${ByteStr(id)})")

    override def lastBlockOpt(): Unit = kill("lastBlockOpt")

    override def blockInfoByHeight(height: Int): Unit = kill(s"blockInfoByHeight($height)")

    override def data(address: Address, key: String): Unit = kill(s"data($address, $key)")

    override def hasData(address: Address): Unit = kill(s"hasData($address)")

    override def resolveAlias(name: String): Unit = kill(s"resolveAlias($name)")

    override def accountBalanceOf(address: Address, assetId: Option[Array[Byte]]): Unit =
      kill(s"accountBalanceOf($address, ${assetId.map(ByteStr(_))})")

    override def accountWavesBalanceOf(address: Address): Unit = kill(s"accountWavesBalanceOf($address)")

    override def accountScript(address: Address): Unit = kill(s"accountScript($address)")

    private def kill(methodName: String) = throw new RuntimeException(s"$methodName is not supported, contact with developers")
  }
}

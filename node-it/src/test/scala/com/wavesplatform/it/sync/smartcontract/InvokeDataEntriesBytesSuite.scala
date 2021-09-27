package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.transactions.{FailedTransactionSuiteLike, OverflowBlock}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.BooleanDataEntry
import com.wavesplatform.test.NumericExt

class InvokeDataEntriesBytesSuite extends BaseTransactionSuite with FailedTransactionSuiteLike[String] with OverflowBlock {
  override protected def waitForHeightArise(): Unit =
    nodes.waitForHeightArise()

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.Ride4DApps.id, 0),
          (BlockchainFeatures.BlockV5.id, 0),
          (BlockchainFeatures.SynchronousCalls.id, 0),
          (BlockchainFeatures.RideV6.id, 0)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  private lazy val (caller, callerAddress) = (firstKeyPair, firstAddress)
  private lazy val (dApp1, dAppAddress1)   = (secondKeyPair, secondAddress)
  private lazy val (dApp2, dAppAddress2)   = (thirdKeyPair, thirdAddress)

  private lazy val dApp3 = sender.createKeyPair()
  private lazy val dApp4 = sender.createKeyPair()
  private lazy val dApp5 = sender.createKeyPair()

  private def data(i: Int, size: Int): String =
    s"""
       | [BinaryEntry("key$i", base64'${ByteStr.fill(size / 2)(1)}' + base64'${ByteStr.fill(size / 2)(1)}')]
     """.stripMargin

  private def dApp1Script(dApp2: Address, size: Int): String =
    TestCompiler(V5)
      .compileContract(
        s"""
         | @Callable(i)
         | func default() = {
         |    strict r = Address(base58'$dApp2').invoke("default", [], [])
         |    ${data(1, size)}
         | }
       """.stripMargin
      )
      .bytes()
      .base64

  private def dApp2Script(dApp3: Address, size: Int): String =
    TestCompiler(V5)
      .compileContract(
        s"""
         | @Callable(i)
         | func default() = {
         |    strict r = Address(base58'$dApp3').invoke("default", [], [])
         |    ${data(2, size)}
         | }
       """.stripMargin
      )
      .bytes()
      .base64

  private def dApp3Script(dApp4: Address, size: Int): String =
    TestCompiler(V5)
      .compileContract(
        s"""
         | @Callable(i)
         | func default() = {
         |   strict r = Address(base58'$dApp4').invoke("default", [], [])
         |   ${data(3, size)}
         | }
       """.stripMargin
      )
      .bytes()
      .base64

  private def dApp4Script(size: Int, fail: Boolean): String =
    TestCompiler(V5)
      .compileContract(
        s"""
         | @Callable(i)
         | func default() = {
         |   # strict c = ${if (fail) List.fill(5)("sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"}
         |   let crash = this.getBoolean("crash").valueOrElse(false)
         |   ${if (fail) s"if (crash) then ${data(4, size)} else []" else data(4, size)}
         | }
       """.stripMargin
      )
      .bytes()
      .base64

  override def beforeAll(): Unit = {
    super.beforeAll()
    Seq(dApp3, dApp4, dApp5).foreach(acc => sender.transfer(sender.keyPair, acc.toAddress.toString, 100.waves))
    sender.waitForHeight(sender.height + 1)
  }

  private val dataSize = ContractLimits.MaxWriteSetSizeInBytes - 9

  test("exceeding 15 Kb leads to reject") {
    sender.setScript(dApp1, Some(dApp1Script(dApp2.toAddress, dataSize)))
    sender.setScript(dApp2, Some(dApp2Script(dApp3.toAddress, dataSize)))
    sender.setScript(dApp3, Some(dApp3Script(dApp4.toAddress, dataSize)))
    sender.setScript(dApp4, Some(dApp4Script(dataSize, fail = false)))
    sender.waitForHeight(sender.height + 1)

    assertApiError(
      sender.invokeScript(caller, dAppAddress1, waitForTx = true),
      AssertiveApiError(
        ScriptExecutionError.Id,
        "Error while executing account-script: Storing data size should not exceed 15360, actual: 20476 bytes",
        matchMessage = true
      )
    )
  }

  test("exceeding 15 Kb leads to fail") {
    sender.setScript(dApp4, Some(dApp4Script(dataSize, fail = true)), waitForTx = true)

    // used to fail invoke
    val priorityData = List(BooleanDataEntry("crash", true))
    val putDataFee   = calcDataFee(priorityData, 1)
    val priorityFee  = putDataFee + invokeFee

    overflowBlock()
    sendTxsAndThenPriorityTx(
      _ => sender.invokeScript(caller, dAppAddress1)._1.id,
      () => sender.putData(dApp4, priorityData, priorityFee).id
    ) { (failed, _) =>
      restApi.assertFailedTxs(failed)
    }
  }
}

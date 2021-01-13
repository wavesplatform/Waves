package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.invokeScrTxSupportedVersions
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions, Recipient}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

class InvokeScriptTransactionGrpcSuite extends GrpcBaseTransactionSuite {
  private val (firstContract, firstContractAddr)   = (firstAcc, firstAddress)
  private val (secondContract, secondContractAddr) = (secondAcc, secondAddress)
  private val thirdContract                        = KeyPair("thirdContract".getBytes("UTF-8"))
  private val thirdContractAddr                    = PBRecipients.create(Address.fromPublicKey(thirdContract.publicKey)).getPublicKeyHash
  private val caller                               = thirdAcc

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        | func foo(a:ByteVector) = {
        |  WriteSet([DataEntry("a", a), DataEntry("sender", inv.caller.bytes)])
        | }
        | @Callable(inv)
        | func emptyKey() = {
        |  WriteSet([DataEntry("", "a")])
        | }
        |
        | @Callable(inv)
        | func default() = {
        |  WriteSet([DataEntry("a", "b"), DataEntry("sender", "senderId")])
        | }
        |
        | @Verifier(tx)
        | func verify() = {
        |    match tx {
        |        case _: TransferTransaction => false
        |        case _ => true
        | }
        |}
        |
        """.stripMargin
    val scriptTextV4 =
      """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        |func foo() = [IntegerEntry("", 1)]
        |
        | @Callable(inv)
        |func bar() = [IntegerEntry("", 2)]
        |
        """.stripMargin
    val script  = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
    val script2 = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3).explicitGet()._1
    miner.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(thirdContractAddr), 10.waves, minFee, waitForTx = true)
    miner.setScript(firstContract, Right(Some(script)), setScriptFee, waitForTx = true)
    miner.setScript(secondContract, Right(Some(script)), setScriptFee, waitForTx = true)
    miner.setScript(thirdContract, Right(Some(script2)), setScriptFee, waitForTx = true)

    val scriptInfo = miner.scriptInfo(firstAddress)
    PBTransactions.toVanillaScript(scriptInfo.scriptBytes) shouldBe Some(script)
  }

  test("dApp caller invokes a function on a dApp") {
    val arg = ByteStr(Array(42: Byte))
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContractAddr else secondContractAddr
      miner.broadcastInvokeScript(
        caller,
        Recipient().withPublicKeyHash(contract),
        Some(FUNCTION_CALL(FunctionHeader.User("foo"), List(CONST_BYTESTR(arg).explicitGet()))),
        fee = 1.waves,
        waitForTx = true
      )

      miner.getDataByKey(contract, "a") shouldBe List(DataEntry("a", DataEntry.Value.BinaryValue(ByteString.copyFrom(arg.arr))))
      miner.getDataByKey(contract, "sender") shouldBe List(
        DataEntry("sender", DataEntry.Value.BinaryValue(ByteString.copyFrom(caller.toAddress.bytes)))
      )
    }
  }

  test("dApp caller invokes a default function on a dApp") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContractAddr else secondContractAddr
      miner.broadcastInvokeScript(
        caller,
        Recipient().withPublicKeyHash(contract),
        functionCall = None,
        fee = 1.waves,
        waitForTx = true
      )
      miner.getDataByKey(contract, "a") shouldBe List(DataEntry("a", DataEntry.Value.StringValue("b")))
      miner.getDataByKey(contract, "sender") shouldBe List(DataEntry("sender", DataEntry.Value.StringValue("senderId")))
    }
  }

  test("verifier works") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract    = if (v < 2) firstContractAddr else secondContractAddr
      val dAppBalance = miner.wavesBalance(contract)
      assertGrpcError(
        miner.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(contract), transferAmount, 1.waves),
        "Transaction is not allowed by account-script",
        Code.INVALID_ARGUMENT
      )
      miner.wavesBalance(contract) shouldBe dAppBalance
    }
  }

  test("not able to set an empty key by InvokeScriptTransaction with version >= 2") {

    assertGrpcError(
      miner.broadcastInvokeScript(
        caller,
        Recipient().withPublicKeyHash(secondContractAddr),
        Some(FUNCTION_CALL(FunctionHeader.User("emptyKey"), List.empty)),
        fee = 1.waves,
        version = TxVersion.V2
      ),
      "Empty keys aren't allowed in tx version >= 2"
    )

    assertGrpcError(
      miner.broadcastInvokeScript(
        caller,
        Recipient().withPublicKeyHash(thirdContractAddr),
        Some(FUNCTION_CALL(FunctionHeader.User("foo"), List.empty)),
        fee = 1.waves,
        version = TxVersion.V2,
        waitForTx = true
      ),
      "Empty keys aren't allowed in tx version >= 2"
    )
  }
}

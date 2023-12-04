package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.sync.smartcontract.invokeScrTxSupportedVersions
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.{DataEntry, PBRecipients, PBTransactions, Recipient}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

class InvokeScriptTransactionGrpcSuite extends GrpcBaseTransactionSuite {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_ => """waves.blockchain.custom.functionality.pre-activated-features.16 = 0""")
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  private val (firstContract, firstContractAddr)   = (firstAcc, firstAddress)
  private val (secondContract, secondContractAddr) = (secondAcc, secondAddress)
  private val thirdContract                        = KeyPair("thirdContract".getBytes("UTF-8"))
  private val thirdContractAddr                    = PBRecipients.create(Address.fromPublicKey(thirdContract.publicKey)).getPublicKeyHash
  private val fourthContract                       = KeyPair("fourthContract".getBytes("UTF-8"))
  private val fourthContractAddr                   = PBRecipients.create(Address.fromPublicKey(fourthContract.publicKey)).getPublicKeyHash
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
    val scriptTextV5 =
      """
        |{-# STDLIB_VERSION 5 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |@Callable(inv)
        |func foo() = {
        |  strict ii = invoke(this, "bar", [1], [])
        |  [IntegerEntry("test1", 1)]
        |}
        |
        |@Callable(inv)
        |func bar(i: Int) = [IntegerEntry("test", 2)]
        |
        """.stripMargin
    val script  = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
    val script2 = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3.latest).explicitGet()._1
    val script3 = ScriptCompiler.compile(scriptTextV5, ScriptEstimatorV3.latest).explicitGet()._1
    sender.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(thirdContractAddr), 10.waves, minFee, waitForTx = true)
    sender.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(fourthContractAddr), 10.waves, minFee, waitForTx = true)
    sender.setScript(firstContract, Right(Some(script)), setScriptFee, waitForTx = true)
    sender.setScript(secondContract, Right(Some(script)), setScriptFee, waitForTx = true)
    sender.setScript(thirdContract, Right(Some(script2)), setScriptFee, waitForTx = true)
    sender.setScript(fourthContract, Right(Some(script3)), setScriptFee, waitForTx = true)

    val scriptInfo = sender.scriptInfo(firstAddress)
    PBTransactions.toVanillaScript(scriptInfo.scriptBytes) shouldBe Some(script)
    scriptInfo.publicKey shouldBe ByteString.copyFrom(firstAcc.publicKey.arr)
  }

  test("dApp caller invokes a nested function on a dApp") {
    val tx = sender.broadcastInvokeScript(
      caller,
      Recipient().withPublicKeyHash(fourthContractAddr),
      Some(FUNCTION_CALL(FunctionHeader.User("foo"), Nil)),
      fee = 1.waves,
      waitForTx = true
    )

    val (_, stateChanges) = sender.stateChanges(tx.id)
    stateChanges.invokes.toString shouldBe s"""List(Invocation(${fourthContract.toAddress},Call(bar,List({"type":"Int","value":1})),List(),StateChangesDetails(List(PutDataResponse(integer,2,test)),List(),List(),List(),List(),List(),None,List())))"""
  }

  test("dApp caller invokes a function on a dApp") {
    val arg = ByteStr(Array(42: Byte))
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContractAddr else secondContractAddr
      sender.broadcastInvokeScript(
        caller,
        Recipient().withPublicKeyHash(contract),
        Some(FUNCTION_CALL(FunctionHeader.User("foo"), List(CONST_BYTESTR(arg).explicitGet()))),
        fee = 1.waves,
        waitForTx = true
      )

      sender.getDataByKey(contract, "a") shouldBe List(DataEntry("a", DataEntry.Value.BinaryValue(ByteString.copyFrom(arg.arr))))
      sender.getDataByKey(contract, "sender") shouldBe List(
        DataEntry("sender", DataEntry.Value.BinaryValue(ByteString.copyFrom(caller.toAddress.bytes)))
      )
    }
  }

  test("dApp caller invokes a default function on a dApp") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContractAddr else secondContractAddr
      sender.broadcastInvokeScript(
        caller,
        Recipient().withPublicKeyHash(contract),
        functionCall = None,
        fee = 1.waves,
        waitForTx = true
      )
      sender.getDataByKey(contract, "a") shouldBe List(DataEntry("a", DataEntry.Value.StringValue("b")))
      sender.getDataByKey(contract, "sender") shouldBe List(DataEntry("sender", DataEntry.Value.StringValue("senderId")))
    }
  }

  test("verifier works") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract    = if (v < 2) firstContractAddr else secondContractAddr
      val dAppBalance = sender.wavesBalance(contract)
      assertGrpcError(
        sender.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(contract), transferAmount, 1.waves),
        "Transaction is not allowed by account-script",
        Code.INVALID_ARGUMENT
      )
      sender.wavesBalance(contract) shouldBe dAppBalance
    }
  }

  test("not able to set an empty key by InvokeScriptTransaction with version >= 2") {

    assertGrpcError(
      sender.broadcastInvokeScript(
        caller,
        Recipient().withPublicKeyHash(secondContractAddr),
        Some(FUNCTION_CALL(FunctionHeader.User("emptyKey"), List.empty)),
        fee = 1.waves,
        version = TxVersion.V2
      ),
      "Empty keys aren't allowed in tx version >= 2"
    )

    assertGrpcError(
      sender.broadcastInvokeScript(
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

package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncGrpcApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.sync.transactions.FailedTransactionSuiteLike
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.DataEntry
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBSignedTransaction, PBTransactions, Recipient}
import com.wavesplatform.state.{BinaryDataEntry, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class FailedTransactionGrpcSuite extends GrpcBaseTransactionSuite with FailedTransactionSuiteLike[PBSignedTransaction] {
  import FailedTransactionSuiteLike.*
  import grpcApi.*

  private val contract     = KeyPair("thirdContract".getBytes("UTF-8"))
  private val contractAddr = PBRecipients.create(Address.fromPublicKey(contract.publicKey)).getPublicKeyHash
  private val caller       = thirdAcc
  private val callerAddr   = PBRecipients.create(Address.fromPublicKey(thirdAcc.publicKey)).getPublicKeyHash

  private val assetAmount    = 1000000000L
  private var smartAsset     = ""
  private var sponsoredAsset = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sender.broadcastTransfer(sender.keyPair, Recipient().withPublicKeyHash(contractAddr), 100.waves, minFee, waitForTx = true)

    smartAsset = PBTransactions
      .vanillaUnsafe(
        sender
          .broadcastIssue(
            contract,
            "Asset",
            assetAmount,
            8,
            reissuable = true,
            issueFee,
            description = "Description",
            script = Right(ScriptCompiler.compile("true", ScriptEstimatorV3(fixOverflow = true, overhead = false)).toOption.map(_._1)),
            waitForTx = true
          )
      )
      .id()
      .toString

    sponsoredAsset = PBTransactions
      .vanillaUnsafe(
        sender
          .broadcastIssue(
            contract,
            "Sponsored Asset",
            assetAmount,
            8,
            reissuable = true,
            issueFee,
            "Description",
            script = Right(None),
            waitForTx = true
          )
      )
      .id()
      .toString

    val scriptTextV4 =
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         |let asset = base58'$smartAsset'
         |
         |@Callable(inv)
         |func tikTok() = {
         |  let action = valueOrElse(getString(this, "tikTok"), "unknown")
         |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} false
         |  if (check) then []
         |  else if (action == "transfer") then [ScriptTransfer(inv.caller, 15, asset)]
         |  else if (action == "issue") then [Issue("new asset", "", 100, 8, true, unit, 0)]
         |  else if (action == "reissue") then [Reissue(asset, 15, true)]
         |  else if (action == "burn") then [Burn(asset, 15)]
         |  else []
         |}
         |
         |@Callable(inv)
         |func transferAndWrite(x: Int) = {
         |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} false
         |  if (check) then []
         |  else if (x % 4 == 0) then [ScriptTransfer(inv.caller, 15, asset), IntegerEntry("n", x)]
         |  else if (x % 4 == 1) then [ScriptTransfer(inv.caller, 15, asset), BooleanEntry("b", x % 2 == 0)]
         |  else if (x % 4 == 2) then [ScriptTransfer(inv.caller, 15, asset), BinaryEntry("bn", toBytes(x))]
         |  else if (x % 4 == 3) then [ScriptTransfer(inv.caller, 15, asset), StringEntry("s", toString(x))]
         |  else []
         |}
         |
         |@Callable(inv)
         |func canThrow() = {
         |  let action = valueOrElse(getString(this, "crash"), "no")
         |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} true
         |
         |  if (action == "yes")
         |  then {
         |    if (check)
         |    then throw("Crashed by dApp")
         |    else throw("Crashed by dApp")
         |  }
         |  else []
         |}
         |
        """.stripMargin
    val script = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3(fixOverflow = true, overhead = false)).explicitGet()._1
    sender.setScript(contract, Right(Some(script)), setScriptFee, waitForTx = true)
  }

  test("InvokeScriptTransaction: insufficient action fees propagates failed transaction") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    for (typeName <- Seq("transfer", "issue", "reissue", "burn")) {
      updateTikTok("unknown", setAssetScriptMinFee)

      overflowBlock()
      sendTxsAndThenPriorityTx(
        _ =>
          sender
            .broadcastInvokeScript(
              caller,
              Recipient().withPublicKeyHash(contractAddr),
              Some(FUNCTION_CALL(FunctionHeader.User("tikTok"), List.empty)),
              fee = invokeFee
            ),
        () => updateTikTok(typeName, priorityFee, waitForTx = false)
      )((txs, _) => assertFailedTxs(txs))
    }
  }

  test("InvokeScriptTransaction: invoke script error in payment asset propagates failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    val paymentAsset = PBTransactions
      .vanillaUnsafe(
        sender
          .broadcastIssue(
            caller,
            "paymentAsset",
            assetAmount,
            8,
            reissuable = true,
            script = Right(ScriptCompiler.compile("true", ScriptEstimatorV3(fixOverflow = true, overhead = false)).toOption.map(_._1)),
            fee = issueFee + smartFee,
            waitForTx = true
          )
      )
      .id()

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)
    updateTikTok("unknown", setAssetScriptMinFee)

    overflowBlock()
    sendTxsAndThenPriorityTx(
      _ =>
        sender
          .broadcastInvokeScript(
            caller,
            Recipient().withPublicKeyHash(contractAddr),
            Some(FUNCTION_CALL(FunctionHeader.User("tikTok"), List.empty)),
            payments = Seq(Amount(ByteString.copyFrom(paymentAsset.arr), 15)),
            fee = invokeFee
          ),
      () => updateAssetScript(result = false, paymentAsset.toString, caller, priorityFee, waitForTx = false)
    )((txs, _) => assertFailedTxs(txs))
  }

  test("InvokeScriptTransaction: sponsored fee on failed transaction should be charged correctly") {
    val invokeFee            = 0.005.waves + smartFee
    val invokeFeeInAsset     = invokeFee / 100000 // assetFee = feeInWaves / feeUnit * sponsorship
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)
    updateTikTok("reissue", setAssetScriptMinFee)

    sender.broadcastSponsorFee(
      contract,
      Some(Amount.of(ByteString.copyFrom(Base58.decode(sponsoredAsset)), 1)),
      sponsorFee + smartFee,
      waitForTx = true
    )
    sender.broadcastTransfer(
      contract,
      Recipient().withPublicKeyHash(callerAddr),
      assetAmount,
      smartMinFee,
      assetId = sponsoredAsset,
      waitForTx = true
    )
    val prevBalance = sender.wavesBalance(contractAddr).regular

    sendTxsAndThenPriorityTx(
      _ =>
        sender.broadcastInvokeScript(
          caller,
          Recipient().withPublicKeyHash(contractAddr),
          Some(FUNCTION_CALL(FunctionHeader.User("tikTok"), List.empty)),
          fee = invokeFeeInAsset,
          feeAssetId = ByteString.copyFrom(Base58.decode(sponsoredAsset))
        ),
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, _) =>
      sender.wavesBalance(contractAddr).regular shouldBe prevBalance - invokeFee * txs.size - priorityFee
      assertFailedTxs(txs)
    }
  }

  test("InvokeScriptTransaction: reject transactions if account script failed") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateTikTok("unknown", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    overflowBlock()
    val prevBalance = sender.wavesBalance(callerAddr).regular
    sendTxsAndThenPriorityTx(
      _ =>
        sender.broadcastInvokeScript(
          caller,
          Recipient().withPublicKeyHash(contractAddr),
          Some(FUNCTION_CALL(FunctionHeader.User("tikTok"), List.empty)),
          fee = invokeFee
        ),
      () =>
        sender.setScript(
          caller,
          Right(
            ScriptCompiler
              .compile(
                s"""
                   |{-# STDLIB_VERSION 3 #-}
                   |{-# CONTENT_TYPE EXPRESSION #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |match (tx) {
                   |case _: InvokeScriptTransaction => false
                   |case _ => true
                   |}
                   |""".stripMargin,
                ScriptEstimatorV3(fixOverflow = true, overhead = false)
              )
              .toOption
              .map(_._1)
          ),
          fee = priorityFee
        )
    ) { (txs, _) =>
      val invalid = assertInvalidTxs(txs)
      sender.wavesBalance(callerAddr).regular shouldBe prevBalance - (txs.size - invalid.size) * invokeFee - priorityFee
      invalid
    }
  }

  def overflowBlock(): Unit = {
    val entries = List.tabulate(4)(n => PBTransactions.toPBDataEntry(BinaryDataEntry("test" + n, ByteStr(Array.fill(32767)(n.toByte)))))
    val fee     = calcDataFee(entries)
    waitForEmptyUtx()
    waitForHeightArise()
    for (_ <- 1 to 8) sender.putData(sender.keyPair, entries, fee)
    waitForEmptyUtx()
  }

  private def calcDataFee(data: List[DataEntry]): Long = {
    val dataSize = data.map(_.toByteArray.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  private def updateTikTok(result: String, fee: Long, waitForTx: Boolean = true): PBSignedTransaction =
    sender.putData(contract, List(StringDataEntry("tikTok", result)).map(PBTransactions.toPBDataEntry), fee = fee, waitForTx = waitForTx)

  override protected def waitForHeightArise(): Unit = sender.waitForHeightArise()

  override protected def nodeConfigs: Seq[Config] = Configs
}

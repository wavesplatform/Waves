package com.wavesplatform.it.sync.grpc

import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.grpc.TransactionsRequest
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.transactions.FailedTransactionSuiteLike
import com.wavesplatform.it.util._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBSignedTransaction, PBTransactions, Recipient}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class FailedTransactionGrpcSuite extends GrpcBaseTransactionSuite with FailedTransactionSuiteLike[PBSignedTransaction] {
  import FailedTransactionSuiteLike._
  import grpcApi._

  private val contract     = KeyPair("thirdContract".getBytes("UTF-8"))
  private val contractAddr = PBRecipients.create(Address.fromPublicKey(contract.publicKey)).getPublicKeyHash
  private val caller       = thirdAcc
  private val callerAddr   = PBRecipients.create(Address.fromPublicKey(thirdAcc.publicKey)).getPublicKeyHash

  private val assetAmount    = 1000000000L
  private var smartAsset     = ""
  private var sponsoredAsset = ""

  private val seller        = firstAcc
  private val buyer         = secondAcc
  private val matcher       = thirdAcc
  private val sellerAddress = firstAddress
  private val buyerAddress  = secondAddress

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
            script = Right(ScriptCompiler.compile("true", ScriptEstimatorV3).toOption.map(_._1)),
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
         |  if (action == "transfer") then [ScriptTransfer(inv.caller, 15, asset)]
         |  else if (action == "issue") then [Issue("new asset", "", 100, 8, true, unit, 0)]
         |  else if (action == "reissue") then [Reissue(asset, true, 15)]
         |  else if (action == "burn") then [Burn(asset, 15)]
         |  else []
         |}
         |
         |@Callable(inv)
         |func transferAndWrite(x: Int) = {
         |  if (x % 4 == 0) then [ScriptTransfer(inv.caller, 15, asset), IntegerEntry("n", x)]
         |  else if (x % 4 == 1) then [ScriptTransfer(inv.caller, 15, asset), BooleanEntry("b", x % 2 == 0)]
         |  else if (x % 4 == 2) then [ScriptTransfer(inv.caller, 15, asset), BinaryEntry("bn", toBytes(x))]
         |  else if (x % 4 == 3) then [ScriptTransfer(inv.caller, 15, asset), StringEntry("s", toString(x))]
         |  else []
         |}
         |
         |@Callable(inv)
         |func canThrow() = {
         |  let action = valueOrElse(getString(this, "crash"), "no")
         |  if (action == "yes") then throw("Crashed by dApp")
         |  else []
         |}
         |
        """.stripMargin
    val script = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3).explicitGet()._1
    sender.setScript(contract, Right(Some(script)), setScriptFee, waitForTx = true)
  }

  test("InvokeScriptTransaction: dApp error propagates failed transaction") {
    val invokeFee    = 0.005.waves
    val priorityData = List(DataEntry("crash", DataEntry.Value.StringValue("yes")))
    val putDataFee   = calcDataFee(priorityData)
    val priorityFee  = putDataFee + invokeFee

    sendTxsAndThenPriorityTx(
      _ =>
        sender
          .broadcastInvokeScript(
            caller,
            Recipient().withPublicKeyHash(contractAddr),
            Some(FUNCTION_CALL(FunctionHeader.User("canThrow"), List.empty)),
            fee = invokeFee
          ),
      () => sender.putData(contract, priorityData, priorityFee, waitForTx = true)
    )((txs, _) => assertFailedTxs(txs))
  }

  test("InvokeScriptTransaction: insufficient action fees propagates failed transaction") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    for (typeName <- Seq("transfer", "issue", "reissue", "burn")) {
      updateTikTok("unknown", setAssetScriptMinFee)

      sendTxsAndThenPriorityTx(
        _ =>
          sender
            .broadcastInvokeScript(
              caller,
              Recipient().withPublicKeyHash(contractAddr),
              Some(FUNCTION_CALL(FunctionHeader.User("tikTok"), List.empty)),
              fee = invokeFee
            ),
        () => updateTikTok(typeName, priorityFee)
      )((txs, _) => assertFailedTxs(txs))
    }
  }

  test("InvokeScriptTransaction: invoking asset script error in action asset propagates failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    for (funcName <- Seq("transfer", "reissue", "burn")) {
      updateTikTok(funcName, setAssetScriptMinFee)
      updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

      sendTxsAndThenPriorityTx(
        _ =>
          sender
            .broadcastInvokeScript(
              caller,
              Recipient().withPublicKeyHash(contractAddr),
              Some(FUNCTION_CALL(FunctionHeader.User("tikTok"), List.empty)),
              fee = invokeFee
            ),
        () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
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
            script = Right(ScriptCompiler.compile("true", ScriptEstimatorV3).toOption.map(_._1)),
            fee = issueFee + smartFee,
            waitForTx = true
          )
      )
      .id()

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)
    updateTikTok("unknown", setAssetScriptMinFee)

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
      () => updateAssetScript(result = false, paymentAsset.toString, caller, priorityFee)
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

  test("InvokeScriptTransaction: account state should not be changed after accepting failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    val initialEntries = List(
      IntegerDataEntry("n", -1),
      BooleanDataEntry("b", value = false),
      BinaryDataEntry("bn", ByteStr(Longs.toByteArray(-1))),
      StringDataEntry("s", "-1")
    ).map(PBTransactions.toPBDataEntry)
    sender.putData(contract, initialEntries, minFee + smartFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    sendTxsAndThenPriorityTx(
      i =>
        sender.broadcastInvokeScript(
          caller,
          Recipient().withPublicKeyHash(contractAddr),
          Some(FUNCTION_CALL(FunctionHeader.User("transferAndWrite"), List(Terms.CONST_LONG(i)))),
          fee = invokeFee
        ),
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, _) =>
      val failed              = assertFailedTxs(txs)
      val lastSuccessEndArg   = txs.size - failed.size
      val lastSuccessStartArg = (lastSuccessEndArg - 3).max(1)

      val lastSuccessWrites =
        Range
          .inclusive(lastSuccessStartArg, lastSuccessEndArg)
          .map {
            case i if i % 4 == 0 => "n"  -> IntegerDataEntry("n", i)
            case i if i % 4 == 1 => "b"  -> BooleanDataEntry("b", i % 2 == 0)
            case i if i % 4 == 2 => "bn" -> BinaryDataEntry("bn", ByteStr(Longs.toByteArray(i)))
            case i if i % 4 == 3 => "s"  -> StringDataEntry("s", i.toString)
          }
          .toMap
          .mapValues(PBTransactions.toPBDataEntry)
      initialEntries.map(entry => entry.key -> entry).toMap.foreach {
        case (key, initial) =>
          sender.getDataByKey(contractAddr, key) shouldBe List(lastSuccessWrites.getOrElse(key, initial))
      }

      sender.stateChanges(TransactionsRequest(transactionIds = failed.map(_.id))).foreach {
        case (_, sc) =>
          sc.issues.size shouldBe 0
          sc.reissues.size shouldBe 0
          sc.burns.size shouldBe 0
          sc.errorMessage shouldBe 'defined
          sc.errorMessage.get.code shouldBe 3
          sc.errorMessage.get.text shouldBe "Transaction is not allowed by token-script"
      }

      failed
    }
  }

  test("InvokeScriptTransaction: reject transactions if account script failed") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateTikTok("unknown", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

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
                   |case t: InvokeScriptTransaction => false
                   |case _ => true
                   |}
                   |""".stripMargin,
                ScriptEstimatorV3
              )
              .toOption
              .map(_._1)
          ),
          fee = priorityFee,
          waitForTx = true
        )
    ) { (txs, _) =>
      val invalid = assertInvalidTxs(txs)
      sender.wavesBalance(callerAddr).regular shouldBe prevBalance - (txs.size - invalid.size) * invokeFee - priorityFee
      invalid
    }
  }

  test("ExchangeTransaction: failed exchange tx when asset script fails") {
    val init = Seq(
      sender.setScript(firstAcc, Right(None), setScriptFee + smartFee),
      sender.setScript(secondAcc, Right(None), setScriptFee + smartFee),
      sender.setScript(thirdAcc, Right(None), setScriptFee + smartFee)
    )

    waitForTxs(init)

    val quantity                                        = 1000000000L
    val initScript: Either[Array[Byte], Option[Script]] = Right(ScriptCompiler.compile("true", ScriptEstimatorV3).toOption.map(_._1))
    val amountAsset                                     = sender.broadcastIssue(seller, "Amount asset", quantity, 8, reissuable = true, issueFee, script = initScript)
    val priceAsset                                      = sender.broadcastIssue(buyer, "Price asset", quantity, 8, reissuable = true, issueFee, script = initScript)
    val sellMatcherFeeAsset                             = sender.broadcastIssue(matcher, "Seller fee asset", quantity, 8, reissuable = true, issueFee, script = initScript)
    val buyMatcherFeeAsset                              = sender.broadcastIssue(matcher, "Buyer fee asset", quantity, 8, reissuable = true, issueFee, script = initScript)

    val preconditions = Seq(
      amountAsset,
      priceAsset,
      sellMatcherFeeAsset,
      buyMatcherFeeAsset
    )

    waitForTxs(preconditions)

    val sellMatcherFeeAssetId = PBTransactions.vanillaUnsafe(sellMatcherFeeAsset).id().toString
    val buyMatcherFeeAssetId  = PBTransactions.vanillaUnsafe(buyMatcherFeeAsset).id().toString

    val transferToSeller = sender.broadcastTransfer(
      matcher,
      Recipient().withPublicKeyHash(sellerAddress),
      quantity,
      fee = minFee + smartFee,
      assetId = sellMatcherFeeAssetId
    )
    val transferToBuyer = sender.broadcastTransfer(
      matcher,
      Recipient().withPublicKeyHash(buyerAddress),
      quantity,
      fee = minFee + smartFee,
      assetId = buyMatcherFeeAssetId
    )

    waitForTxs(Seq(transferToSeller, transferToBuyer))

    val amountAssetId  = PBTransactions.vanillaUnsafe(amountAsset).id().toString
    val priceAssetId   = PBTransactions.vanillaUnsafe(priceAsset).id().toString
    val assetPair      = AssetPair.createAssetPair(amountAssetId, priceAssetId).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setAssetScriptFee + smartFee + fee * 10

    val allCases =
      Seq((amountAssetId, seller), (priceAssetId, buyer), (sellMatcherFeeAssetId, matcher), (buyMatcherFeeAssetId, matcher))

    for ((invalidScriptAsset, owner) <- allCases) {
      val txsSend = (_: Int) => {
        val tx = PBTransactions.protobuf(
          mkExchange(buyer, seller, matcher, assetPair, fee, buyMatcherFeeAssetId, sellMatcherFeeAssetId, buyMatcherFee, sellMatcherFee)
        )
        sender.broadcast(tx.transaction.get, tx.proofs)
      }
      sendTxsAndThenPriorityTx(
        txsSend,
        () => updateAssetScript(result = false, invalidScriptAsset, owner, priorityFee)
      )((txs, _) => assertFailedTxs(txs))
      updateAssetScript(result = true, invalidScriptAsset, owner, setAssetScriptFee + smartFee)
    }
  }

  test("ExchangeTransaction: invalid exchange tx when account script fails") {
    val quantity            = 1000000000L
    val amountAsset         = sender.broadcastIssue(seller, "Amount asset", quantity, 8, reissuable = true, issueFee)
    val priceAsset          = sender.broadcastIssue(buyer, "Price asset", quantity, 8, reissuable = true, issueFee)
    val sellMatcherFeeAsset = sender.broadcastIssue(matcher, "Seller fee asset", quantity, 8, reissuable = true, issueFee)
    val buyMatcherFeeAsset  = sender.broadcastIssue(matcher, "Buyer fee asset", quantity, 8, reissuable = true, issueFee)

    val preconditions = Seq(
      amountAsset,
      priceAsset,
      sellMatcherFeeAsset,
      buyMatcherFeeAsset
    )

    waitForTxs(preconditions)

    val sellMatcherFeeAssetId = PBTransactions.vanillaUnsafe(sellMatcherFeeAsset).id().toString
    val buyMatcherFeeAssetId  = PBTransactions.vanillaUnsafe(buyMatcherFeeAsset).id().toString

    val transferToSeller = sender.broadcastTransfer(
      matcher,
      Recipient().withPublicKeyHash(sellerAddress),
      quantity,
      fee = minFee + smartFee,
      assetId = sellMatcherFeeAssetId
    )
    val transferToBuyer = sender.broadcastTransfer(
      matcher,
      Recipient().withPublicKeyHash(buyerAddress),
      quantity,
      fee = minFee + smartFee,
      assetId = buyMatcherFeeAssetId
    )

    waitForTxs(Seq(transferToSeller, transferToBuyer))

    val amountAssetId  = PBTransactions.vanillaUnsafe(amountAsset).id().toString
    val priceAssetId   = PBTransactions.vanillaUnsafe(priceAsset).id().toString
    val assetPair      = AssetPair.createAssetPair(amountAssetId, priceAssetId).get
    val fee            = 0.003.waves + smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setScriptFee + smartFee + fee * 10

    val allCases = Seq(seller, buyer, matcher)
    allCases.foreach(address => updateAccountScript(None, address, setScriptFee + smartFee))

    for (invalidAccount <- allCases) {
      val txsSend = (_: Int) => {
        val tx = PBTransactions.protobuf(
          mkExchange(buyer, seller, matcher, assetPair, fee, buyMatcherFeeAssetId, sellMatcherFeeAssetId, buyMatcherFee, sellMatcherFee)
        )
        sender.broadcast(tx.transaction.get, tx.proofs)
      }

      sendTxsAndThenPriorityTx(
        txsSend,
        () => updateAccountScript(Some(false), invalidAccount, priorityFee)
      )((txs, _) => assertInvalidTxs(txs))
      updateAccountScript(None, invalidAccount, setScriptFee + smartFee)
    }
  }

  private def calcDataFee(data: List[DataEntry]): Long = {
    val dataSize = data.map(_.toByteArray.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  private def updateTikTok(result: String, fee: Long): PBSignedTransaction =
    sender.putData(contract, List(StringDataEntry("tikTok", result)).map(PBTransactions.toPBDataEntry), fee = fee, waitForTx = true)

  private def waitForTxs(txs: Seq[PBSignedTransaction]): Unit = {
    txs.foreach(tx => sender.waitForTransaction(PBTransactions.vanillaUnsafe(tx).id().toString))
  }

  override protected def waitForHeightArise(): Unit = sender.waitForHeightArise()

  override protected def nodeConfigs: Seq[Config] = Configs
}

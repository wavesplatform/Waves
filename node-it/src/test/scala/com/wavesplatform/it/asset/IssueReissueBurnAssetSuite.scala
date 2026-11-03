package com.wavesplatform.it.asset

import com.google.protobuf.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.{AssetDoesNotExist, TransactionDoesNotExist}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.{
  AsyncHttpApi,
  BurnInfoResponse,
  IssueInfoResponse,
  ReissueInfoResponse,
  StateChangesDetails,
  SyncGrpcApi,
  TransactionInfo
}
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.{BaseFreeSpec, Docker}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBSignedTransaction, PBTransactions}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import monix.execution.atomic.AtomicInt
import org.scalatest.Assertion

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** Issue/reissue/burn through plain transactions and dApp actions. One node serves both REST and gRPC: each test broadcasts through one of them, and
  * every result is checked through both, so the two APIs are covered without running the same chain activity twice.
  */
class IssueReissueBurnAssetSuite extends BaseFreeSpec {
  import IssueReissueBurnAssetSuite.*
  import com.wavesplatform.it.NodeConfigs.*

  override val nodeConfigs: Seq[Config] = Seq(BiggestMiner.quorum(0))

  override protected def createDocker: Docker =
    new Docker(suiteConfig = ConfigFactory.parseString(s"waves.extensions = [${Docker.GrpcExtension}]"), tag = getClass.getSimpleName)

  private val initialWavesBalance = 100.waves
  private val setScriptPrice      = 0.01.waves
  private val accountCounter      = AtomicInt(1000)

  private val CallableMethod    = "@Callable"
  private val TransactionMethod = "Transaction"

  private val simpleNonreissuableAsset = Asset("Simple", "SimpleAsset", "description", 100500, reissuable = false, 8, 0)
  private val simpleReissuableAsset    = Asset("Reissuable", "ReissuableAsset", "description", 100000000, reissuable = true, 3, 0)
  private val longMaxAsset             = Asset("Long max", "name", "", Long.MaxValue, reissuable = true, 8, Long.MaxValue)
  private val longMinAsset             = Asset("Long min", "name" * 4, "A" * 1000, Long.MaxValue, reissuable = true, 0, Long.MinValue)
  private val nftAsset                 = Asset("NFT", "NFTAsset", "description", 1, reissuable = false, 0, 0)

  // Each method is broadcast through a different API, so between them both APIs send plain transactions and invocations.
  for ((method, api) <- Seq(CallableMethod -> Api.Rest, TransactionMethod -> Api.Grpc)) s"Asset Issue/Reissue/Burn via $method, sent over $api" - {
    val isCallable = method == CallableMethod

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset, longMaxAsset, longMinAsset))
      s"${data.assetType} asset could be issued in callable" in {
        val acc = createDapp(api, script(data))

        val fee  = invocationCost(if (isNft(data)) 0 else 1)
        val txId = issue(api, acc, method, data, fee)

        validateIssuedAssets(acc, txId, data, method = method)
        assertWavesBalance(acc, initialWavesBalance - setScriptPrice - fee)
      }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset)) s"${data.assetType} asset could be partially burned" in {
      val acc            = createDapp(api, script(data))
      val fee            = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue        = issue(api, acc, method, data, fee)
      val assetId        = validateIssuedAssets(acc, txIssue, data, method = method)
      val burnQuantity   = 1000
      val remainQuantity = data.quantity - burnQuantity

      burn(api, acc, TransactionMethod, assetId, burnQuantity / 2)
      burn(api, acc, CallableMethod, assetId, burnQuantity / 2)
      assertAsset(assetId)(remainQuantity)
      assertBalance(acc, assetId, remainQuantity)
    }

    for (data <- Seq(simpleNonreissuableAsset, simpleReissuableAsset, nftAsset, longMaxAsset)) s"${data.assetType} could be fully burned" in {
      val acc     = createDapp(api, script(data))
      val fee     = invocationCost(if (isNft(data)) 0 else 1)
      val txIssue = issue(api, acc, method, data, fee)
      val assetId = validateIssuedAssets(acc, txIssue, data, method = method)

      assertBalance(acc, assetId, data.quantity)
      val txId = burn(api, acc, method, assetId, data.quantity)

      assertAsset(assetId)(0)
      assertBalance(acc, assetId, 0)

      if (isCallable) assertStateChanges(txId) { sd =>
        sd.burns should matchPattern { case Seq(BurnInfoResponse(`assetId`, data.quantity)) =>
        }
      }

      if (data.reissuable) {
        reissue(api, acc, method, assetId, data.quantity, reissuable = true)
        assertAsset(assetId)(data.quantity)
        assertBalance(acc, assetId, data.quantity)
      }
    }

    "Reissuable asset could be reissued" in {
      val acc             = createDapp(api, script(simpleReissuableAsset))
      val txIssue         = issue(api, acc, method, simpleReissuableAsset, invocationCost(1))
      val assetId         = validateIssuedAssets(acc, txIssue, simpleReissuableAsset, method = method)
      val initialQuantity = simpleReissuableAsset.quantity
      val addedQuantity   = 100500

      reissue(api, acc, TransactionMethod, assetId, addedQuantity / 2, reissuable = true)
      reissue(api, acc, CallableMethod, assetId, addedQuantity / 2, reissuable = false)

      assertAsset(assetId)(initialQuantity + addedQuantity, reissuable = Some(!simpleReissuableAsset.reissuable))
      assertBalance(acc, assetId, initialQuantity + addedQuantity)
    }

    "Non-reissuable asset could not be reissued" in {
      val acc     = createDapp(api, script(simpleNonreissuableAsset))
      val txIssue = issue(api, acc, method, simpleNonreissuableAsset, invocationCost(1))
      val assetId = validateIssuedAssets(acc, txIssue, simpleNonreissuableAsset, method = method)
      assertRejected(api, "Asset is not reissuable")(reissue(api, acc, method, assetId, 100500, reissuable = false, checkStateChanges = false))
    }

    "Issued asset could be sponsored" in {
      val assetAcc = createDapp(api, script(simpleReissuableAsset))
      val nftAcc   = createDapp(api, script(nftAsset))
      val assetId  = validateIssuedAssets(assetAcc, issue(api, assetAcc, method, simpleReissuableAsset), simpleReissuableAsset, method = method)
      val nftId    = validateIssuedAssets(nftAcc, issue(api, nftAcc, method, nftAsset), nftAsset, method = method)

      sponsor(api, assetAcc, assetId, simpleReissuableAsset.quantity / 2)
      sponsor(api, nftAcc, nftId, 1)

      assertSponsorship(assetId, simpleReissuableAsset.quantity / 2)
      assertSponsorship(nftId, 1)
    }
  }

  // A rejected broadcast leaves no trace on chain, so each of these checks the rejection through both APIs at no cost.
  "Restrictions in " + CallableMethod - {
    val method = CallableMethod

    "Issue two identical assets with the same nonce (one invocation) should produce an error" in {
      val acc = createDapp(Api.Rest, script(simpleNonreissuableAsset))
      for (api <- Api.values)
        assertRejected(api, " is already issued")(invoke(api, acc, "issue2Assets", fee = invocationCost(2)))
    }

    "Issue two identical assets with the same nonce (different invocations) should not produce an error" in {
      val acc      = createDapp(Api.Rest, script(simpleNonreissuableAsset))
      val txIssue1 = issue(Api.Rest, acc, method, simpleNonreissuableAsset, invocationCost(1))
      val txIssue2 = issue(Api.Grpc, acc, method, simpleNonreissuableAsset, invocationCost(1))
      validateIssuedAssets(acc, txIssue1, simpleNonreissuableAsset, method = method)
      validateIssuedAssets(acc, txIssue2, simpleNonreissuableAsset, method = method)
    }

    "Reissuing more than Long.Max should produce an error" in {
      val acc             = createDapp(Api.Rest, script(simpleReissuableAsset))
      val assetId         = validateIssuedAssets(acc, issue(Api.Rest, acc, method, simpleReissuableAsset), simpleReissuableAsset, method = method)
      val initialQuantity = simpleReissuableAsset.quantity

      for (api <- Api.values)
        assertRejected(api, "Asset total value overflow")(
          reissue(api, acc, method, assetId, Long.MaxValue - initialQuantity + 1, reissuable = true, checkStateChanges = false)
        )
    }

    "Burning more than current asset count should produce an error" in {
      val acc     = createDapp(Api.Rest, script(simpleReissuableAsset))
      val assetId = validateIssuedAssets(acc, issue(Api.Rest, acc, method, simpleReissuableAsset), simpleReissuableAsset, method = method)

      for (api <- Api.values)
        assertRejected(api, "Accounts balance errors")(
          invoke(api, acc, "transferAndBurn", assetId = assetId, count = simpleReissuableAsset.quantity / 2 + 1, wait = false)
        )
    }

    "Reissuing NFT asset should produce an error" in {
      val acc     = createDapp(Api.Rest, script(nftAsset))
      val assetId = validateIssuedAssets(acc, issue(Api.Rest, acc, method, nftAsset), nftAsset, method = method)

      for (api <- Api.values)
        assertRejected(api, "Asset is not reissuable")(reissue(api, acc, method, assetId, 2, reissuable = true, checkStateChanges = false))
    }

    "Reissuing after setting isReissuiable to falser inside one invocation should produce an error" in {
      val acc     = createDapp(Api.Rest, script(simpleReissuableAsset))
      val assetId = validateIssuedAssets(acc, issue(Api.Rest, acc, method, simpleReissuableAsset), simpleReissuableAsset, method = method)

      for (api <- Api.values)
        assertRejected(api, "Asset is not reissuable")(
          invoke(api, acc, "reissueAndReissue", assetId = assetId, count = 1000, fee = invocationCost(1))
        )

      assertAsset(assetId)(simpleReissuableAsset.quantity)
    }
  }

  "State changes" - {
    "No issue" in {
      val script =
        """
          |{-# STDLIB_VERSION 4 #-}
          |{-# SCRIPT_TYPE ACCOUNT #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Callable(i)
          |func nooperation() = {
          |  [
          |  ]
          |}""".stripMargin

      val acc = createDapp(Api.Rest, script)
      assertStateChanges(invoke(Api.Rest, acc, "nooperation")) { sc =>
        sc.issues shouldBe empty
        sc.burns shouldBe empty
        sc.reissues shouldBe empty
      }
    }

    "Two assets" in {
      val asset = Asset("test", "name", "description", 10, reissuable = true, 8, 1)
      val acc   = createDapp(Api.Grpc, script(asset))

      val asset1Id = validateIssuedAssets(acc, issue(Api.Grpc, acc, CallableMethod, asset), asset)
      reissue(Api.Grpc, acc, CallableMethod, asset1Id, 10, reissuable = true)
      burn(Api.Grpc, acc, CallableMethod, asset1Id, 5)
      assertAsset(asset1Id)(15, reissuable = Some(true))
      assertBalance(acc, asset1Id, 15)

      val asset2Id = validateIssuedAssets(acc, issue(Api.Grpc, acc, CallableMethod, asset), asset)
      burn(Api.Grpc, acc, CallableMethod, asset2Id, 5)
      reissue(Api.Grpc, acc, CallableMethod, asset2Id, 10, reissuable = false)
      assertAsset(asset2Id)(15, reissuable = Some(false))
      assertBalance(acc, asset2Id, 15)
    }

    "NFT burning removes it from list" in {
      val acc     = createDapp(Api.Rest, script(nftAsset))
      val assetId = validateIssuedAssets(acc, issue(Api.Rest, acc, CallableMethod, nftAsset, invocationCost(1)), nftAsset)
      assertNfts(acc, 2)(_ shouldBe Seq(assetId))
      burn(Api.Rest, acc, CallableMethod, assetId, 1)
      assertNfts(acc, 1)(_ shouldBe empty)
    }

    // Distribution is a REST-only endpoint and is served for the last solid block, hence the block waits here.
    "distribution works" in {
      val acc   = createDapp(Api.Rest, script(simpleReissuableAsset))
      val asset = issueValidated(acc, simpleReissuableAsset)
      invoke(Api.Rest, acc, "transferAndBurn", assetId = asset, count = 100)
      nodes.waitForHeightArise()
      sender.assetDistribution(asset).map { case (a, v) => a.toString -> v } shouldBe Map(
        miner.address  -> 100L,
        addressOf(acc) -> (simpleReissuableAsset.quantity - 200)
      )
      reissue(Api.Rest, acc, CallableMethod, asset, 400, reissuable = false)
      invoke(Api.Rest, acc, "transferAndBurn", assetId = asset, count = 100)
      nodes.waitForHeightArise()
      sender.assetDistribution(asset).map { case (a, v) => a.toString -> v } shouldBe Map(
        miner.address  -> 200L,
        addressOf(acc) -> simpleReissuableAsset.quantity
      )
    }

    // Rollback exists only in the REST debug API; what it leaves behind is checked through both.
    "rollback works" in {
      val acc        = createDapp(Api.Rest, script(simpleReissuableAsset))
      val addressStr = addressOf(acc)
      val assetA     = issueValidated(acc, simpleReissuableAsset)

      sender.transactionsByAddress(addressStr, 100).flatMap(_.stateChanges) should matchPattern {
        case Seq(StateChangesDetails(Nil, Nil, Seq(issue), Nil, Nil, Nil, None, Nil)) if issue.name == simpleReissuableAsset.name =>
      }

      val height = nodes.waitForHeightArise()
      nodes.waitForHeightArise()
      val txId = invoke(Api.Rest, acc, "reissueIssueAndNft", assetId = assetA, fee = invocationCost(1))

      val (assetNft, assetB) = sender
        .stateChanges(txId)
        .stateChanges
        .map { scd =>
          val nft   = scd.issues.find(_.name == nftAsset.name).get.assetId
          val asset = scd.issues.find(_.name == simpleReissuableAsset.name).get.assetId
          (nft, asset)
        }
        .get

      nodes.waitForHeightArise()

      nodes.rollback(height, returnToUTX = false)

      sender.transactionsByAddress(addressStr, 100).flatMap(_.stateChanges) should matchPattern {
        case Seq(StateChangesDetails(Nil, Nil, Seq(issue), Nil, Nil, Nil, None, Nil)) if issue.name == simpleReissuableAsset.name =>
      }
      assertApiError(sender.stateChanges(txId), TransactionDoesNotExist)

      assertAsset(assetA)(simpleReissuableAsset.quantity, reissuable = Some(true))
      assertApiError(sender.assetsDetails(assetB), AssetDoesNotExist(IssuedAsset(ByteStr.decodeBase58(assetB).get)))
      assertApiError(sender.assetsDetails(assetNft), AssetDoesNotExist(IssuedAsset(ByteStr.decodeBase58(assetNft).get)))
      SyncGrpcApi.assertGrpcError(grpc.assetInfo(assetB), expectedCode = io.grpc.Status.Code.NOT_FOUND)
      SyncGrpcApi.assertGrpcError(grpc.assetInfo(assetNft), expectedCode = io.grpc.Status.Code.NOT_FOUND)

      assertBalance(acc, assetA, simpleReissuableAsset.quantity)
      // assetB no longer exists, which the REST balance assertion reports as an API error rather than a zero balance.
      sender.assetBalance(addressStr, assetB).balance shouldBe 0L
      grpc.assetsBalance(pbAddress(acc), Seq(assetB)).getOrElse(assetB, 0L) shouldBe 0L
      sender.assetsBalance(addressStr).balances.map(_.assetId).toSet shouldBe Set(assetA)
      assertNfts(acc, 10)(_ shouldBe empty)
    }

    // Reissue 100 and burn 50 make the quantity change observable, so the checks can tell an applied invocation from one that was not.
    "liquid block works" in {
      val acc   = createDapp(Api.Grpc, script(simpleReissuableAsset))
      val asset = issueValidated(acc, simpleReissuableAsset, Api.Grpc)
      val txId  = invoke(Api.Grpc, acc, "reissueIssueAndNft", assetId = asset, fee = invocationCost(1))
      def checks(): Unit = {
        assertStateChanges(txId) { sd =>
          sd.issues should have size 2
          sd.burns should have size 1
          sd.reissues should have size 1
        }
        assertAsset(asset)(simpleReissuableAsset.quantity + 50)
        assertBalance(acc, asset, simpleReissuableAsset.quantity + 50)
        sender.assetsBalance(addressOf(acc)).balances should have size 2
        assertNfts(acc, 10)(_ should have size 1)
      }
      checks()
      nodes.waitForHeightArise()
      checks()
    }
  }

  private def grpc = SyncGrpcApi.NodeExtGrpc(sender)

  private def addressOf(acc: KeyPair): String = acc.toAddress.toString

  private def pbAddress(acc: KeyPair): ByteString = SyncGrpcApi.keyPairAsBytes(acc)

  private def idOf(tx: PBSignedTransaction): String = PBTransactions.vanilla(tx, unsafe = false).explicitGet().id().toString

  // The default 1s poll would add most of a second to each of the ~165 waits.
  private def mined(txId: String): TransactionInfo = sync(AsyncHttpApi.NodeAsyncHttpApi(sender).waitForTransaction(txId, 200.millis))

  // Every wait goes through this, not through `mined`: the miner can orphan its own newest microblock (see "Key block forged on a stale parent"
  // in docs/notes/mining.md), and for about 200ms its transactions are gone, which breaks both a broadcast validated against them and a read
  // of their results. Under the next key block, still at the same height, a transaction stays.
  @tailrec private def settled(txId: String): Unit = {
    val height = mined(txId).height
    sender.waitFor[Int](s"height > $height")(_.height.toInt, _ > height, 200.millis)
    if (mined(txId).height != height) settled(txId)
  }

  def createDapp(api: Api, scriptParts: String*): KeyPair = {
    val acc            = TxHelpers.signer(accountCounter.getAndIncrement())
    val compiledScript = ScriptCompiler.compile(scriptParts.mkString(" "), ScriptEstimatorV2).explicitGet()._1
    val setScript      = TxHelpers.setScript(acc = acc, script = compiledScript, fee = setScriptFee, version = 1.toByte)

    // The set-script fee comes out of the transfer, and every invocation needs the script.
    api match {
      case Api.Rest =>
        settled(miner.transfer(sender.keyPair, addressOf(acc), initialWavesBalance, setScriptFee * 2).id)
        settled(miner.signedBroadcast(setScript.json()).id)
      case Api.Grpc =>
        settled(idOf(grpc.broadcastTransfer(sender.keyPair, PBRecipients.create(acc.toAddress), initialWavesBalance, setScriptFee * 2)))
        settled(idOf(grpc.signedBroadcast(PBTransactions.protobuf(setScript))))
    }

    acc
  }

  def invoke(
      api: Api,
      dApp: KeyPair,
      function: String,
      wait: Boolean = true,
      assetId: String = "",
      count: Long = 1,
      isReissuable: Boolean = true,
      fee: Long = invokeFee
  ): String = {
    def asset = CONST_BYTESTR(ByteStr.decodeBase58(assetId).get).explicitGet()
    val args: List[EXPR] = function match {
      case "transferAndBurn" | "burnAsset" | "reissueAndReissue" => List(asset, CONST_LONG(count))
      case "reissueIssueAndNft" | "process11actions"             => List(asset)
      case "reissueAsset"                                        => List(asset, CONST_BOOLEAN(isReissuable), CONST_LONG(count))
      case _                                                     => Nil
    }

    val txId = api match {
      case Api.Rest => miner.invokeScript(dApp, addressOf(dApp), func = Some(function), args = args, fee = fee)._1.id
      case Api.Grpc =>
        idOf(
          grpc.broadcastInvokeScript(dApp, PBRecipients.create(dApp.toAddress), Some(FUNCTION_CALL(FunctionHeader.User(function), args)), fee = fee)
        )
    }

    if (wait) settled(txId)
    txId
  }

  def assertRejected(api: Api, message: String)(f: => Any): Assertion = api match {
    case Api.Rest => assertApiError(f)(e => e.message should include(message))
    case Api.Grpc => SyncGrpcApi.assertGrpcError(f, Pattern.quote(message))
  }

  // REST serves state changes three ways (plain, amounts as strings, by address) and gRPC a fourth; all must agree.
  def assertStateChanges(txId: String)(f: StateChangesDetails => Unit): Unit = {
    withClue("REST: ")(f(sender.stateChanges(txId).stateChanges.get))
    withClue("REST, amounts as strings: ")(f(sender.stateChanges(txId, amountsAsStrings = true).stateChanges.get))
    withClue("REST, by address: ") {
      val senderAddress = sender.transactionInfo[TransactionInfo](txId).sender.get
      val byAddress     = sender.transactionsByAddress(senderAddress, 100).find(_.id == txId)
      byAddress shouldBe defined
      f(byAddress.get.stateChanges.get)
    }
    withClue("gRPC: ") {
      val (transaction, details) = grpc.stateChanges(txId)
      transaction.chainId shouldBe 'I'.toByte
      f(details)
    }
  }

  def validateIssue(issue: IssueInfoResponse, data: Asset): Unit = {
    issue.name shouldBe data.name
    issue.description shouldBe data.description
    issue.decimals shouldBe data.decimals
    issue.isReissuable shouldBe data.reissuable
    issue.quantity shouldBe data.quantity
  }

  def assertAsset(assetId: String)(quantity: Long, reissuable: Option[Boolean] = None): Unit = {
    withClue("REST: ") {
      val info = sender.assetsDetails(assetId)
      info.quantity shouldBe quantity
      reissuable.foreach(info.reissuable shouldBe _)
    }
    withClue("gRPC: ") {
      val info = grpc.assetInfo(assetId)
      info.totalVolume shouldBe quantity
      reissuable.foreach(info.reissuable shouldBe _)
    }
  }

  def assertBalance(acc: KeyPair, assetId: String, expected: Long): Unit = {
    withClue("REST: ")(sender.assertAssetBalance(addressOf(acc), assetId, expected))
    withClue("gRPC: ")(grpc.assertAssetBalance(pbAddress(acc), assetId, expected))
  }

  def assertWavesBalance(acc: KeyPair, expected: Long): Unit = {
    withClue("REST: ")(sender.balanceDetails(addressOf(acc)).regular shouldBe expected)
    withClue("gRPC: ")(grpc.wavesBalance(pbAddress(acc)).regular shouldBe expected)
  }

  def assertNfts(acc: KeyPair, limit: Int)(f: Seq[String] => Unit): Unit = {
    withClue("REST: ")(f(sender.nftList(addressOf(acc), limit).map(_.assetId)))
    withClue("gRPC: ")(f(grpc.nftList(pbAddress(acc), limit).map(r => Base58.encode(r.assetId.toByteArray))))
  }

  def assertSponsorship(assetId: String, minFee: Long): Unit = {
    withClue("REST: ")(sender.assetsDetails(assetId).minSponsoredAssetFee shouldBe Some(minFee))
    withClue("gRPC: ")(grpc.assetInfo(assetId).sponsorship shouldBe minFee)
  }

  def validateIssuedAssets(account: KeyPair, txId: String, data: Asset, nth: Int = -1, method: String = CallableMethod): String = {
    val assetId = method match {
      case CallableMethod => invokeAssetId(txId, nth)
      case _              => txId
    }

    val tx = sender.transactionInfo[TransactionInfo](txId)
    withClue("REST: ") {
      val assetInfo = sender.assetsDetails(assetId)
      assetInfo.originTransactionId shouldBe txId
      assetInfo.issueTimestamp shouldBe tx.timestamp
      assetInfo.issuer shouldBe tx.sender.get
      assetInfo.name shouldBe data.name
      assetInfo.description shouldBe data.description
      assetInfo.reissuable shouldBe data.reissuable
      assetInfo.decimals shouldBe data.decimals
      assetInfo.quantity shouldBe data.quantity
      assetInfo.scriptDetails shouldBe None
    }
    withClue("gRPC: ") {
      val assetInfo = grpc.assetInfo(assetId)
      assetInfo.issuer shouldBe ByteString.copyFrom(account.publicKey.arr)
      assetInfo.name shouldBe data.name
      assetInfo.description shouldBe data.description
      assetInfo.reissuable shouldBe data.reissuable
      assetInfo.decimals shouldBe data.decimals
      assetInfo.totalVolume shouldBe data.quantity
      assetInfo.script shouldBe None
    }

    assertBalance(account, assetId, data.quantity)

    if (method == CallableMethod) assertStateChanges(txId) { sd =>
      val issue = if (nth == -1) sd.issues.head else sd.issues(nth)
      validateIssue(issue, data)
    }

    assetId
  }

  def invokeAssetId(txId: String, nth: Int = -1): String = {
    val issues = sender.stateChanges(txId).stateChanges.get.issues
    (if (nth == -1) issues.head else issues(nth)).assetId
  }

  def issueValidated(account: KeyPair, data: Asset, api: Api = Api.Rest): String =
    validateIssuedAssets(account, issue(api, account, CallableMethod, data), data)

  def issue(api: Api, account: KeyPair, method: String, data: Asset, fee: Long = invocationCost(1)): String = method match {
    case CallableMethod =>
      val txId = invoke(api, account, "issueAsset", fee = fee)
      assertStateChanges(txId) { sd =>
        (sd.issues: @unchecked) match {
          case Seq(issue) => validateIssue(issue, data)
        }
      }
      txId

    case _ =>
      val txId = api match {
        case Api.Rest => sender.issue(account, data.name, data.description, data.quantity, data.decimals, reissuable = data.reissuable, fee = fee).id
        case Api.Grpc =>
          idOf(
            grpc.broadcastIssue(
              account,
              data.name,
              data.quantity,
              data.decimals,
              reissuable = data.reissuable,
              fee = fee,
              description = data.description
            )
          )
      }
      settled(txId)
      txId
  }

  def reissue(
      api: Api,
      account: KeyPair,
      method: String,
      assetId: String,
      quantity: Long,
      reissuable: Boolean,
      fee: Long = invokeFee,
      checkStateChanges: Boolean = true
  ): String = method match {
    case CallableMethod =>
      val txId = invoke(api, account, "reissueAsset", assetId = assetId, count = quantity, isReissuable = reissuable, fee = fee)
      if (checkStateChanges)
        assertStateChanges(txId) { sd =>
          sd.reissues should matchPattern { case Seq(ReissueInfoResponse(`assetId`, `reissuable`, `quantity`)) =>
          }
        }
      txId

    case _ =>
      val txId = api match {
        case Api.Rest => sender.reissue(account, assetId, quantity, reissuable, version = TxVersion.V2).id
        case Api.Grpc => idOf(grpc.broadcastReissue(account, fee, assetId, quantity, reissuable, version = TxVersion.V2))
      }
      settled(txId)
      txId
  }

  def burn(api: Api, account: KeyPair, method: String, assetId: String, quantity: Long, fee: Long = invokeFee): String = method match {
    case CallableMethod =>
      val txId = invoke(api, account, "burnAsset", assetId = assetId, count = quantity, fee = fee)
      assertStateChanges(txId) { sd =>
        sd.burns should matchPattern { case Seq(BurnInfoResponse(`assetId`, `quantity`)) =>
        }
      }
      txId

    case _ =>
      val txId = api match {
        case Api.Rest => sender.burn(account, assetId, quantity, fee = invokeFee, version = TxVersion.V2).id
        case Api.Grpc => idOf(grpc.broadcastBurn(account, assetId, quantity, fee = invokeFee, version = TxVersion.V2))
      }
      settled(txId)
      txId
  }

  def sponsor(api: Api, account: KeyPair, assetId: String, minFee: Long): String = {
    val txId = api match {
      case Api.Rest => sender.sponsorAsset(account, assetId, minFee, sponsorFee).id
      case Api.Grpc =>
        idOf(grpc.broadcastSponsorFee(account, Some(Amount.of(ByteString.copyFrom(Base58.decode(assetId)), minFee)), sponsorFee))
    }
    settled(txId)
    txId
  }

  def isNft(asset: Asset): Boolean = {
    asset.quantity == 1 && asset.decimals == 0
  }

  def invocationCost(aCount: Int, isSmartAcc: Boolean = true, sPCount: Int = 0, sAinActions: Int = 0): Long = {
    0.005.waves + (if (isSmartAcc) 0.004.waves else 0L) + 0.004.waves * sPCount + 0.004.waves * sAinActions + 1.waves * aCount
  }

  def script(asset: Asset, function: String = ""): String = {
    def createIssueParams(asset: Asset) = s""""${asset.name}","${asset.description}",${asset.quantity}, ${asset.decimals},${asset.reissuable},unit"""
    val issueParams                     = createIssueParams(asset)

    s"""
       |{-# STDLIB_VERSION 4 #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |{-# CONTENT_TYPE DAPP #-}
       |
       |@Callable (i)
       |func issue2Assets() = {
       |  [
       |    Issue($issueParams, 0),
       |    Issue($issueParams, 0)
       |  ]
       |}
       |
       |@Callable (i)
       |func issue10Assets() = {
       |  [
       |    Issue($issueParams, 0),
       |    Issue($issueParams, 1),
       |    Issue($issueParams, 2),
       |    Issue($issueParams, 3),
       |    Issue($issueParams, 4),
       |    Issue($issueParams, 5),
       |    Issue($issueParams, 6),
       |    Issue($issueParams, 7),
       |    Issue($issueParams, 8),
       |    Issue($issueParams, 9)
       |  ]
       |}
       |
       |@Callable (i)
       |func issue11Assets() = {
       |  [
       |    Issue($issueParams, 1),
       |    Issue($issueParams, 2),
       |    Issue($issueParams, 3),
       |    Issue($issueParams, 4),
       |    Issue($issueParams, 5),
       |    Issue($issueParams, 6),
       |    Issue($issueParams, 7),
       |    Issue($issueParams, 8),
       |    Issue($issueParams, 9),
       |    Issue($issueParams, 10),
       |    Issue($issueParams, 11)
       |  ]
       |}
       |
       |@Callable (i) func process11actions(a: ByteVector) = {
       |  [
       |    Issue($issueParams, 0),
       |    Reissue(a, 1000, true),
       |    Issue($issueParams, 2),
       |    Issue($issueParams, 3),
       |    Reissue(a, 2000, true),
       |    Reissue(a, 2000, true),
       |    Reissue(a, 3000, true),
       |    Burn(a, 6212),
       |    Reissue(a, 2000, true),
       |    Issue($issueParams, 1),
       |    Burn(a, 12311)
       |  ]
       |}
       |
       |@Callable (i) func issueAsset() = [Issue($issueParams, ${asset.nonce})]
       |
       |@Callable (i) func burnAsset(a: ByteVector, q: Int) = [Burn(a, q)]
       |
       |@Callable (i) func reissueAsset(a: ByteVector, r: Boolean, q: Int) = [Reissue(a, q, r)]
       |
       |@Callable (i) func reissueAndReissue(a: ByteVector, rq: Int) = [Reissue(a, rq, false), Reissue(a, rq, false)]
       |
       |@Callable(i)
       |func transferAndBurn(a: ByteVector, q: Int) = {
       |  [
       |    ScriptTransfer(Address(fromBase58String("${miner.address}")), q, a),
       |    Burn(a, q)
       | ]
       |}
       |
       |@Callable(i)
       |func reissueIssueAndNft(a: ByteVector) = {
       |  [
       |    Issue($issueParams, ${asset.nonce + 1}),
       |    Reissue(a, 100, true),
       |    Burn(a, 50),
       |    Issue(${createIssueParams(nftAsset)}, 1)
       |  ]
       |}
       |
       |$function
       |
       """.stripMargin
  }
}

object IssueReissueBurnAssetSuite {
  enum Api { case Rest, Grpc }
}

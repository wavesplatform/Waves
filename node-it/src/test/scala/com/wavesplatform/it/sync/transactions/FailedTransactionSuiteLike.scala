package com.wavesplatform.it.sync.transactions

import com.google.protobuf.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{ApplicationStatus, TransactionsByIdRequest, TransactionStatus => PBTransactionStatus}
import com.wavesplatform.api.http.ApiError.TransactionDoesNotExist
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.TransactionStatus
import com.wavesplatform.it.{Node, NodeConfigs}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{Asset, TxVersion}
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsObject

import scala.concurrent.duration._

trait FailedTransactionSuiteLike[T] extends ScorexLogging { _: Matchers =>
  protected def waitForHeightArise(): Unit
  protected def sender: Node

  /**
    * Sends `max-transactions-in-micro-block` * 2 transactions and then sends priority transaction.
    * @param t transaction sender
    * @param pt priority transaction sender
    * @param checker transactions checker (will be executed twice - immediately after emptying the utx pool and then after height arising)
    */
  def sendTxsAndThenPriorityTx[S](t: Int => T, pt: () => T)(
      checker: (Seq[T], T) => Seq[S]
  ): Seq[S] = {
    val maxTxsInMicroBlock = sender.config.getInt("waves.miner.max-transactions-in-micro-block")
    val txs                = (1 to maxTxsInMicroBlock * 2).map(i => t(i))
    val priorityTx         = pt()
    waitForEmptyUtx()
    waitForHeightArise()
    waitForEmptyUtx()
    checker(txs, priorityTx) // hardened
  }

  object restApi {
    import com.wavesplatform.it.api.SyncHttpApi.{NodeExtSync, assertApiError}

    /**
      * Checks that transactions contain failed and returns them.
      */
    def assertFailedTxs(txs: Seq[String]): Seq[TransactionStatus] = {
      val statuses = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.id) < txs.indexOf(s.id) }
      all(statuses.map(_.status)) shouldBe "confirmed"
      all(statuses.map(_.applicationStatus.isDefined)) shouldBe true

      statuses.foreach { s =>
        (sender.transactionInfo[JsObject](s.id) \ "applicationStatus").asOpt[String] shouldBe s.applicationStatus
      }

      val failed = statuses.dropWhile(s => s.applicationStatus.contains("succeeded"))
      failed.size should be > 0

      all(failed.flatMap(_.applicationStatus)) shouldBe "script_execution_failed"

      val failedIdsByHeight = failed.groupBy(_.height.get).view.mapValues(_.map(_.id))

      failedIdsByHeight.foreach {
        case (h, ids) =>
          sender.blockAt(h).transactions.map(_.id) should contain allElementsOf ids
          sender.blockSeq(h, h).head.transactions.map(_.id) should contain allElementsOf ids
          sender.blockById(sender.blockAt(h).id).transactions.map(_.id) should contain allElementsOf ids
          sender.blockSeqByAddress(sender.address, h, h).head.transactions.map(_.id) should contain allElementsOf ids

          val liquidBlock         = sender.lastBlock()
          val maxHeightWithFailed = failedIdsByHeight.keys.max
          if (liquidBlock.height == maxHeightWithFailed) {
            liquidBlock.transactions.map(_.id) should contain allElementsOf failedIdsByHeight(maxHeightWithFailed)
          }
      }
      failed
    }

    /**
      * Checks that transactions contain invalid and returns them.
      */
    def assertInvalidTxs(txs: Seq[String]): Seq[TransactionStatus] = {
      val statuses = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.id) < txs.indexOf(s.id) }

      val invalid = statuses.dropWhile(s => s.applicationStatus.contains("succeeded"))
      invalid.size should be > 0

      invalid.foreach { s =>
        assertApiError(sender.transactionInfo[JsObject](s.id), TransactionDoesNotExist)
      }

      all(invalid.map(_.status)) shouldBe "not_found"
      all(invalid.map(_.applicationStatus)) shouldBe None

      invalid
    }

    def updateAssetScript(result: Boolean, asset: String, owner: KeyPair, fee: Long, waitForTx: Boolean = true): String = {
      sender
        .setAssetScript(
          asset,
          owner,
          fee,
          Some(
            ScriptCompiler
              .compile(
                s"""
                   |match tx {
                   |  case _: SetAssetScriptTransaction => true
                   |  case _ =>
                   |    let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} false
                   |    if (check) then false else $result
                   |}
                   |""".stripMargin,
                ScriptEstimatorV3(fixOverflow = true, overhead = false)
              )
              .explicitGet()
              ._1
              .bytes()
              .base64
          ),
          waitForTx = waitForTx
        )
        .id
    }

    def updateAccountScript(result: Option[Boolean], account: KeyPair, fee: Long, waitForTx: Boolean = true): String = {
      sender
        .setScript(
          account,
          result.map { r =>
            ScriptCompiler
              .compile(
                s"""
                   |{-# STDLIB_VERSION 3 #-}
                   |{-# CONTENT_TYPE EXPRESSION #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |match (tx) {
                   |  case _: SetScriptTransaction => true
                   |  case _ =>
                   |    let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} false
                   |    if (check) then false else $r
                   |}
                   |""".stripMargin,
                ScriptEstimatorV3(fixOverflow = true, overhead = false)
              )
              .explicitGet()
              ._1
              .bytes()
              .base64

          },
          fee = fee,
          waitForTx = waitForTx
        )
        .id
    }
  }

  object grpcApi {
    import com.wavesplatform.it.api.SyncGrpcApi._

    /**
      * Checks that transactions contain failed and returns them.
      */
    def assertFailedTxs(txs: Seq[PBSignedTransaction]): Seq[PBTransactionStatus] = {
      val txsIds = txs.map(PBTransactions.vanillaUnsafe).map(tx => ByteString.copyFrom(tx.id().arr))
      val req    = TransactionsByIdRequest(txs.map(PBTransactions.vanillaUnsafe).map(tx => ByteString.copyFrom(tx.id().arr)))

      val statuses = sender.getStatuses(req).sortWith { case (f, s) => txsIds.indexOf(f.id) < txsIds.indexOf(s.id) }
      all(statuses.map(_.status)) shouldBe PBTransactionStatus.Status.CONFIRMED
      all(statuses.map(_.applicationStatus)) should not be ApplicationStatus.UNKNOWN

      val failed = statuses.dropWhile(s => s.applicationStatus == ApplicationStatus.SUCCEEDED)

      failed.size should be > 0
      all(failed.map(_.applicationStatus)) shouldBe ApplicationStatus.SCRIPT_EXECUTION_FAILED

      val failedIdsByHeight = failed.groupBy(_.height.toInt).view.mapValues(_.map(_.id))

      failedIdsByHeight.foreach {
        case (h, ids) =>
          sender.blockAt(h).transactionData.map(_.id()) should contain allElementsOf ids.map(bs => ByteStr(bs.toByteArray))
          sender.blockSeq(h, h).head.transactionData.map(_.id()) should contain allElementsOf ids.map(bs => ByteStr(bs.toByteArray))
      }

      failed
    }

    /**
      * Checks that transactions contain invalid and returns them.
      */
    def assertInvalidTxs(txs: Seq[PBSignedTransaction]): Seq[PBTransactionStatus] = {
      val txsIds   = txs.map(PBTransactions.vanillaUnsafe).map(tx => ByteString.copyFrom(tx.id().arr))
      val req      = TransactionsByIdRequest(txs.map(PBTransactions.vanillaUnsafe).map(tx => ByteString.copyFrom(tx.id().arr)))
      val statuses = sender.getStatuses(req).sortWith { case (f, s) => txsIds.indexOf(f.id) < txsIds.indexOf(s.id) }

      val invalid = statuses.dropWhile(s => s.status == PBTransactionStatus.Status.CONFIRMED)
      all(invalid.map(_.status)) shouldBe PBTransactionStatus.Status.NOT_EXISTS
      all(invalid.map(_.applicationStatus)) shouldBe ApplicationStatus.UNKNOWN

      invalid
    }

    def updateAssetScript(result: Boolean, asset: String, owner: KeyPair, fee: Long, waitForTx: Boolean = true): PBSignedTransaction = {
      sender
        .setAssetScript(
          owner,
          asset,
          Right(
            Some(
              ScriptCompiler
                .compile(
                  s"""
                   |match tx {
                   |  case _: SetAssetScriptTransaction => true
                   |  case _ =>
                   |    let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} false
                   |    if (check) then false else $result
                   |}
                   |""".stripMargin,
                  ScriptEstimatorV3(fixOverflow = true, overhead = false)
                )
                .explicitGet()
                ._1
            )
          ),
          fee,
          waitForTx = waitForTx
        )
    }

    def updateAccountScript(result: Option[Boolean], account: KeyPair, fee: Long, waitForTx: Boolean = true): PBSignedTransaction = {
      sender
        .setScript(
          account,
          Right(
            result.flatMap { r =>
              ScriptCompiler
                .compile(
                  s"""
                   |{-# STDLIB_VERSION 3 #-}
                   |{-# CONTENT_TYPE EXPRESSION #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |match (tx) {
                   |  case _: SetScriptTransaction => true
                   |  case _ => $r
                   |}
                   |""".stripMargin,
                  ScriptEstimatorV3(fixOverflow = true, overhead = false)
                )
                .toOption
                .map(_._1)
            }
          ),
          fee = fee
        )
    }
  }

  def waitForEmptyUtx(): Unit = {
    import com.wavesplatform.it.api.SyncHttpApi._

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)
  }
}

object FailedTransactionSuiteLike {
  def mkExchange(
      buyer: KeyPair,
      seller: KeyPair,
      matcher: KeyPair,
      assetPair: AssetPair,
      fee: Long,
      buyMatcherFeeAsset: String,
      sellMatcherFeeAsset: String,
      buyMatcherFee: Long,
      sellMatcherFee: Long
  ): ExchangeTransaction = {
    val ts   = System.currentTimeMillis()
    val bmfa = Asset.fromString(Some(buyMatcherFeeAsset))
    val smfa = Asset.fromString(Some(sellMatcherFeeAsset))
    val buy  = Order.buy(Order.V4, buyer, matcher.publicKey, assetPair, 100, 100, ts, ts + Order.MaxLiveTime, buyMatcherFee, bmfa)
    val sell = Order.sell(Order.V4, seller, matcher.publicKey, assetPair, 100, 100, ts, ts + Order.MaxLiveTime, sellMatcherFee, smfa)
    ExchangeTransaction
      .signed(
        TxVersion.V3,
        matcher.privateKey,
        buy,
        sell,
        buy.amount,
        buy.price,
        buy.matcherFee,
        sell.matcherFee,
        fee,
        ts
      )
      .explicitGet()
  }

  val configForMinMicroblockAge: Config = ConfigFactory.parseString(s"""
     |waves.miner.min-micro-block-age = 7
     |waves.miner.max-transactions-in-micro-block = 1
     |""".stripMargin)

  val Configs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw(s"waves.miner.max-transactions-in-micro-block = 50"))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()
}

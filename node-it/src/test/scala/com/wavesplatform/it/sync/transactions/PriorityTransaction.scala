package com.wavesplatform.it.sync.transactions

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.{ApplicationStatus, TransactionsByIdRequest, TransactionStatus => PBTransactionStatus}
import com.wavesplatform.api.http.ApiError.TransactionDoesNotExist
import com.wavesplatform.http.DebugMessage
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.TransactionStatus
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.Matchers
import play.api.libs.json.JsObject

import scala.concurrent.duration._

trait PriorityTransaction { _: Matchers =>
  protected def waitForHeightArise(): Unit
  protected def sender: Node

  /**
    * Sends `max-transactions-in-micro-block` * 2 transactions and then sends priority transaction.
    * @param t transaction sender
    * @param pt priority transaction sender
    * @param checker transactions checker (will be executed twice - immediately after emptying the utx pool and then after height arising)
    */
  def sendTxsAndThenPriorityTx[T, S](t: Int => T, pt: () => T)(
      checker: Seq[T] => Seq[S]
  ): Unit = {
    import com.wavesplatform.it.api.SyncHttpApi._

    val maxTxsInMicroBlock = sender.config.getInt("waves.miner.max-transactions-in-micro-block")
    val txs                = (1 to maxTxsInMicroBlock * 2).map(i => t(i))
    val priorityTx         = pt()
    waitForEmptyUtx()
    sender.printDebugMessage(DebugMessage(s"Priority transaction: $priorityTx"))

    checker(txs) // liquid
    waitForHeightArise()
    checker(txs) // hardened
  }

  object restApi {
    import com.wavesplatform.it.api.SyncHttpApi._

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

      val failed = statuses.dropWhile(s => s.applicationStatus.contains("succeed"))
      failed.size should be > 0

      all(failed.flatMap(_.applicationStatus)) shouldBe "scriptExecutionFailed"

      failed
    }

    /**
      * Checks that transactions contain invalid and returns them.
      */
    def assertInvalidTxs(txs: Seq[String]): Seq[TransactionStatus] = {
      val statuses = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.id) < txs.indexOf(s.id) }

      val invalid = statuses.dropWhile(s => s.applicationStatus.contains("succeed"))
      invalid.size should be > 0

      invalid.foreach { s =>
        assertApiError(sender.transactionInfo[JsObject](s.id), TransactionDoesNotExist)
      }

      all(invalid.map(_.status)) shouldBe "not_found"
      all(invalid.map(_.applicationStatus)) shouldBe None

      invalid
    }

    def updateAssetScript(result: Boolean, asset: String, owner: String, fee: Long): String = {
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
                   |  case tx: SetAssetScriptTransaction => true
                   |  case _ => $result
                   |}
                   |""".stripMargin,
                ScriptEstimatorV3
              )
              .right
              .get
              ._1
              .bytes()
              .base64
          ),
          waitForTx = true
        )
        .id
    }

    def updateAccountScript(result: Option[Boolean], account: String, fee: Long): String = {
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
                   |  case t: SetScriptTransaction => true
                   |  case _ => $r
                   |}
                   |""".stripMargin,
                ScriptEstimatorV3
              )
              .right
              .get
              ._1
              .bytes()
              .base64

          },
          fee = fee,
          waitForTx = true
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

      val failed = statuses.dropWhile(s => s.applicationStatus == ApplicationStatus.SUCCEED)

      failed.size should be > 0
      all(failed.map(_.applicationStatus)) shouldBe ApplicationStatus.SCRIPT_EXECUTION_FAILED

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

    def updateAssetScript(result: Boolean, asset: String, owner: KeyPair, fee: Long): PBSignedTransaction = {
      sender
        .setAssetScript(
          owner,
          asset,
          Right(
            ScriptCompiler
              .compile(
                s"""
                   |match tx {
                   |  case tx: SetAssetScriptTransaction => true
                   |  case _ => $result
                   |}
                   |""".stripMargin,
                ScriptEstimatorV3
              )
              .toOption
              .map(_._1)
          ),
          fee,
          waitForTx = true
        )
    }

    def updateAccountScript(result: Option[Boolean], account: KeyPair, fee: Long): PBSignedTransaction = {
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
                   |  case t: SetScriptTransaction => true
                   |  case _ => $r
                   |}
                   |""".stripMargin,
                  ScriptEstimatorV3
                )
                .toOption
                .map(_._1)
            }
          ),
          fee = fee,
          waitForTx = true
        )
    }
  }

  def waitForEmptyUtx(): Unit = {
    import com.wavesplatform.it.api.SyncHttpApi._

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)
  }
}

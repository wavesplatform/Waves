package com.wavesplatform.data

import java.time.{LocalDate, ZoneId}

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.utils.ScorexLogging
import okhttp3.{MediaType, OkHttpClient, Request, RequestBody}
import play.api.libs.json.Json

import scala.util.control.NonFatal

object VolkMain extends App with ScorexLogging {
  def sendNotification(channel: String, height: Int, txId: ByteStr, typeStr: String, text: String): Unit = {
    val httpClient = new OkHttpClient.Builder().build()
    val msg =
      s"""New script discovered!
        |Type: $typeStr
        |Height: *$height*
        |Transaction: https://wavesexplorer.com/tx/$txId""".stripMargin

    val json = Json.obj(
      "text"        -> msg,
      "attachments" -> Json.arr(Json.obj("text" -> text))
    )

    val request = new Request.Builder()
      .url(channel)
      .post(RequestBody.create(MediaType.parse("application/json"), json.toString()))
      .build()

    val response = httpClient.newCall(request).execute()
    require(response.isSuccessful, s"Not success: $response")
    log.info(s"Notified: $response")
  }

  val startHeight = sys.env.get("VOLK_HEIGHT").map(_.toInt)
  val channels    = sys.env.get("VOLK_CHANNELS").toSeq.flatMap(_.split(',')).filter(_.nonEmpty)
  val pa          = new PollingAgent
  pa.start(_.foreach {
    case e @ BlockchainUpdated(_, height, BlockchainUpdated.Update.Append(BlockchainUpdated.Append(_, _, _, body)))
        if startHeight.forall(_ <= height) =>
      log.info(s"Event at $height: ${body.getClass.getName}")

      val transactions = body match {
        case Body.Block(value)      => value.transactions
        case Body.MicroBlock(value) => value.getMicroBlock.transactions
        case Body.Empty             => Nil
      }

      val byTx = transactions.map { tx =>
        tx -> (tx.getTransaction.data match {
          case Data.Issue(value)          => value.script
          case Data.SetScript(value)      => value.script
          case Data.SetAssetScript(value) => value.script
          case _                          => None
        })
      }

      byTx.collect {
        case (tx, Some(script)) =>
          log.info(s"Script $script")
          val parsedScript = PBTransactions.toVanillaScript(script)
          val (text, _)    = Script.decompile(parsedScript)
          val vtx          = PBTransactions.vanillaUnsafe(tx)
          val txId         = vtx.id()
          val isAsset      = vtx.builder.typeId != SetScriptTransaction.typeId
          val complexity   = Script.estimate(parsedScript, ScriptEstimatorV3).getOrElse(0L)
          Stats.update(isAsset, complexity.toInt)
          channels.foreach(sendNotification(_, height, txId, if (isAsset) "Asset" else "Account", text))
      }

      Stats.publish(channels)

    case e =>
      log.warn(s"Event ignored at ${e.height}: ${e.update.getClass.getName}")
  })

  object Stats {
    var lastTime: LocalDate = LocalDate.now()
    var accounts: Int       = 0
    var assets: Int         = 0
    var complexity: Int     = 0

    def update(isAsset: Boolean, complexity: Int): Unit = {
      if (isAsset) assets += 1
      else accounts += 1
      this.complexity += complexity
    }

    def publish(channels: Seq[String]): Unit = {
      val now = LocalDate.now(ZoneId.of("Europe/Moscow"))
      if (now.compareTo(this.lastTime.plusDays(1L)) >= 0) {
        try {
          channels.foreach(sendStats)
          this.accounts = 0
          this.assets = 0
          this.complexity = 0
          this.lastTime = now
        } catch { case NonFatal(e) => log.error("Error sending stats", e) }
      }
    }

    def sendStats(channel: String): Unit = {
      val httpClient = new OkHttpClient.Builder().build()
      val msg =
        s"""*Daily stats*
           |Total scripts: ${accounts + assets}
           |Account scripts: $accounts
           |Asset scripts: $assets
           |Total complexity: $complexity""".stripMargin

      val json = Json.obj(
        "text" -> msg
      )

      val request = new Request.Builder()
        .url(channel)
        .post(RequestBody.create(MediaType.parse("application/json"), json.toString()))
        .build()

      val response = httpClient.newCall(request).execute()
      require(response.isSuccessful, s"Not success: $response")
      log.info(s"Notified: $response")
    }
  }
}

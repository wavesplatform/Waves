package com.wavesplatform

import java.io.{FileOutputStream, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Try

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.transaction.{AuthorizedTransaction, Transaction, TxValidationError}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils.ScorexLogging
import org.influxdb.dto.Point

private class ResponsivenessLogs(csvPrefix: String, metricName: String) extends ScorexLogging {
  import ResponsivenessLogs.TxEvent

  // noinspection ScalaStyle
  private case class MetricSnapshot(point: Point.Builder = null, nano: Long = System.nanoTime(), millis: Long = System.currentTimeMillis())

  private case class TxState(
      received: Long,
      lastReceived: Long,
      firstMined: Option[MetricSnapshot],
      lastMined: Option[MetricSnapshot],
      failed: Option[MetricSnapshot],
      miningAttempt: Int,
      height: Int
  )
  private val stateMap = mutable.AnyRefMap.empty[ByteStr, TxState]

  def writeEvent(
      height: Int,
      tx: Transaction,
      eventType: TxEvent,
      reason: Option[ValidationError] = None,
      writeCsv: Boolean = false,
      retentionPolicy: String = ""
  ): Unit =
    Try(synchronized {
      val reasonClass = reason match {
        case Some(value)                       => value.getClass.getSimpleName
        case _ if eventType == TxEvent.Expired => "Expired"
        case _                                 => "Unknown"
      }

      val isAlreadyInTheState = eventType == TxEvent.Invalidated && reasonClass != "AlreadyInTheState"

      def writeMetrics(): Unit = {
        def toMillis(ns: Long) = Duration.fromNanos(ns).toMillis
        val nowNanos           = System.nanoTime()

        if (eventType == TxEvent.Received)
          stateMap(tx.id()) = stateMap.get(tx.id()) match {
            case None =>
              TxState(nowNanos, nowNanos, None, None, None, 0, height)

            case Some(state) =>
              state.copy(
                lastReceived = nowNanos,
                lastMined = None,
                miningAttempt = if (state.lastMined.nonEmpty) state.miningAttempt + 1 else state.miningAttempt
              )
          }

        val basePoint = Point
          .measurement(metricName)
          .tag("id", tx.id().toString)
          .tag("event", eventType.toString.toLowerCase)
          .addField("type", tx.tpe.id)
          .addField("height", height)

        if (eventType == TxEvent.Mined) {
          stateMap.get(tx.id()).foreach { case TxState(received, lastReceived, firstMined, _, _, attempt, _) =>
            val delta     = toMillis(nowNanos - received)
            val lastDelta = toMillis(nowNanos - lastReceived)
            log.trace(s"Neutrino mining time for ${tx.id()} (attempt #$attempt): $delta ms ($lastDelta from last recv)")

            val snapshot = MetricSnapshot(basePoint.addField("time-to-mine", delta).addField("time-to-last-mine", lastDelta), nowNanos)
            stateMap(tx.id()) = TxState(
              received,
              lastReceived,
              firstMined.orElse(Some(snapshot)),
              Some(snapshot),
              None,
              attempt,
              height
            )
          }
        } else if (eventType == TxEvent.Expired || (eventType == TxEvent.Invalidated && !isAlreadyInTheState)) {
          stateMap.get(tx.id()).foreach { case st @ TxState(received, lastReceived, firstMined, _, _, _, _) =>
            val delta     = toMillis(nowNanos - received)
            val lastDelta = toMillis(nowNanos - lastReceived)
            log.trace(s"Neutrino fail time for ${tx.id()}: $delta ms")

            val baseFailedPoint = basePoint
              .tag("reason", reasonClass)
              .addField("time-to-fail", delta)
              .addField("time-to-last-fail", lastDelta)

            val failedPoint = firstMined match {
              case Some(ms) =>
                val ffDelta    = toMillis(nowNanos - ms.nano)
                val firstDelta = toMillis(ms.nano - received)
                baseFailedPoint
                  .addField("time-to-first-mine", firstDelta)
                  .addField("time-to-finish-after-first-mining", ffDelta)

              case None =>
                baseFailedPoint
            }

            stateMap(tx.id()) = st.copy(failed = Some(MetricSnapshot(failedPoint)))
          }
        }

        def writeMinedPoint(received: Long, firstMined: Option[MetricSnapshot], mined: MetricSnapshot) = {
          val ffDelta    = toMillis(firstMined.fold(0L)(ms => mined.nano - ms.nano))
          val firstDelta = toMillis(firstMined.fold(0L)(ms => ms.nano - received))
          val finalPoint = mined.point
            .addField("time-to-first-mine", firstDelta)
            .addField("time-to-finish-after-first-mining", ffDelta)
          log.trace(s"Writing mined responsiveness point: ${finalPoint.build()}")
          Metrics.write(finalPoint, mined.millis)
        }

        stateMap.toVector.collect {
          case (txId, TxState(received, _, firstMined, Some(mined), _, _, h)) if (h + 5) <= height =>
            writeMinedPoint(received, firstMined, mined)
            stateMap -= txId

          case (txId, TxState(received, _, firstMined, Some(mined), Some(MetricSnapshot(point, _, millis)), _, h)) if (h + 100) <= height =>
            log.trace(s"Writing failed responsiveness point: ${point.build()}")
            Metrics.write(point, millis)
            writeMinedPoint(received, firstMined, mined)
            stateMap -= txId

          case (txId, st) if (st.height + 1000) < height =>
            stateMap -= txId
        }
      }

      def writeCsvLog(prefix: String): Unit = {
        def escape(s: String): String = s.replaceAll("\\r", "\\\\r").replaceAll("\\n", "\\\\n")

        val date       = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)
        val fileStream = new FileOutputStream(s"${sys.props("waves.directory")}/$prefix-events-$date.csv", true)
        val pw         = new PrintWriter(fileStream)
        val reasonEscaped = reason match {
          case Some(see: TxValidationError.ScriptExecutionError)        => s"ScriptExecutionError(${escape(see.message)})"
          case Some(_: TxValidationError.TransactionNotAllowedByScript) => "TransactionNotAllowedByScript"
          case Some(err)                                                => escape(err.toString)
          case None                                                     => ""
        }
        val txType    = tx.tpe.id
        val timestamp = System.currentTimeMillis()
        val txJson    = if (eventType == TxEvent.Expired || eventType == TxEvent.Invalidated) tx.json().toString() else ""
        val logLine   = s"${tx.id()};$eventType;$height;$txType;$timestamp;$reasonClass;$reasonEscaped;$txJson"
        // log.info(logLine)
        try pw.println(logLine)
        finally pw.close()
      }

      Metrics.withRetentionPolicy(retentionPolicy)(writeMetrics())
      if (writeCsv && !isAlreadyInTheState) writeCsvLog(csvPrefix)
    }).failed.foreach(log.error("Error writing responsiveness metrics", _))
}

object ResponsivenessLogs {
  var enableMetrics   = false
  var enableCsv       = false
  var retentionPolicy = ""

  private val neutrino = new ResponsivenessLogs("neutrino", "neutrino")
  private val ordinary = new ResponsivenessLogs("tx", "blockchain-responsiveness")

  type TxEvent = TxEvent.Value
  object TxEvent extends Enumeration {
    val Received, Mined, Expired, Invalidated = Value
  }

  def isNeutrino(tx: Transaction): Boolean = {
    val txAddrs = tx match {
      case is: InvokeScriptTransaction =>
        Seq(is.senderAddress) ++ (is.dApp match {
          case a: Address => Seq(a)
          case _          => Nil
        })
      case a: AuthorizedTransaction => Seq(a.sender.toAddress)
      case _                        => Nil
    }

    val neutrinoAddrs = Set(
      "3PC9BfRwJWWiw9AREE2B3eWzCks3CYtg4yo",
      "3PG2vMhK5CPqsCDodvLGzQ84QkoHXCJ3oNP",
      "3P5Bfd58PPfNvBM2Hy8QfbcDqMeNtzg7KfP",
      "3P4PCxsJqMzQBALo8zANHtBDZRRquobHQp7",
      "3PNikM6yp4NqcSU8guxQtmR5onr2D4e8yTJ"
    )

    txAddrs.map(_.toString).exists(neutrinoAddrs)
  }

  def writeEvent(height: Int, tx: Transaction, eventType: TxEvent, reason: Option[ValidationError] = None): Unit =
    if (!enableMetrics) ()
    else {
      if (isNeutrino(tx)) neutrino.writeEvent(height, tx, eventType, reason, enableCsv, retentionPolicy)
      ordinary.writeEvent(height, tx, eventType, reason, enableCsv, retentionPolicy)
    }
}

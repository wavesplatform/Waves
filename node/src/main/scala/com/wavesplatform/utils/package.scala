package com.wavesplatform

import java.security.SecureRandom

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.ByteStr._
import org.joda.time.Duration
import org.joda.time.format.PeriodFormat
import play.api.libs.json._

import scala.annotation.tailrec

package object utils extends ScorexLogging {

  private val BytesMaxValue  = 256
  private val Base58MaxValue = 58

  private val BytesLog = math.log(BytesMaxValue)
  private val BaseLog  = math.log(Base58MaxValue)

  def base58Length(byteArrayLength: Int): Int = math.ceil(BytesLog / BaseLog * byteArrayLength).toInt

  def forceStopApplication(reason: ApplicationStopReason = Default): Unit =
    new Thread(() => {
      System.exit(reason.code)
    }, "waves-platform-shutdown-thread").start()

  def humanReadableSize(bytes: Long, si: Boolean = true): String = {
    val (baseValue, unitStrings) =
      if (si)
        (1000, Vector("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"))
      else
        (1024, Vector("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))

    @tailrec
    def getExponent(curBytes: Long, baseValue: Int, curExponent: Int = 0): Int =
      if (curBytes < baseValue) curExponent
      else {
        val newExponent = 1 + curExponent
        getExponent(curBytes / (baseValue * newExponent), baseValue, newExponent)
      }

    val exponent   = getExponent(bytes, baseValue)
    val divisor    = Math.pow(baseValue, exponent)
    val unitString = unitStrings(exponent)

    f"${bytes / divisor}%.1f $unitString"
  }

  def humanReadableDuration(duration: Long): String = {
    val d = new Duration(duration)
    PeriodFormat.getDefault.print(d.toPeriod)
  }

  implicit class Tap[A](a: A) {
    def tap(g: A => Unit): A = {
      g(a)
      a
    }
  }

  def randomBytes(howMany: Int = 32): Array[Byte] = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }

  implicit val byteStrWrites: Format[ByteStr] = new Format[ByteStr] {
    override def writes(o: ByteStr): JsValue = JsString(o.base58)

    override def reads(json: JsValue): JsResult[ByteStr] = json match {
      case JsString(v) => decodeBase58(v).fold(e => JsError(s"Error parsing base58: ${e.getMessage}"), b => JsSuccess(b))
      case _           => JsError("Expected JsString")
    }
  }
}

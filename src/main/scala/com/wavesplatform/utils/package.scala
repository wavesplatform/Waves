package com.wavesplatform

import java.io.File
import java.nio.file.Files

import com.wavesplatform.state2.VersionableStorage
import monix.execution.UncaughtExceptionReporter
import org.fusesource.leveldbjni.JniDBFactory
import org.iq80.leveldb.{DB, Options}
import scorex.utils.ScorexLogging

import scala.util.Try

package object utils extends ScorexLogging {


  def base58Length(byteArrayLength: Int): Int = math.ceil(math.log(256) / math.log(58) * byteArrayLength).toInt

  def createStore(file: File): DB = {
    val options = new Options()
    options.createIfMissing(true)

    file.getParentFile.mkdirs()
    val db = JniDBFactory.factory.open(file, options)
    db
  }

  def createWithStore[A <: AutoCloseable with VersionableStorage](storeFile: Option[File], f: => A, pred: A => Boolean = (_: A) => true, deleteExisting: Boolean = false): Try[A] = Try {
    for (fileToDelete <- storeFile if deleteExisting) Files.delete(fileToDelete.toPath)
    val a = f
    if (a.isVersionValid && pred(a)) a else storeFile match {
      case Some(file) =>
        log.info(s"Re-creating file store at $file")
        a.close()
        Files.delete(file.toPath)
        val newA = f
        require(pred(newA), "store is inconsistent")
        newA
      case None => throw new IllegalArgumentException("in-memory store is corrupted")
    }
  }

  def forceStopApplication(reason: ApplicationStopReason = Default): Unit = new Thread(() => {
    System.exit(reason.code)
  }, "waves-platform-shutdown-thread").start()

  def humanReadableSize(bytes: Long, si: Boolean = true): String = {
    val (baseValue, unitStrings) =
      if (si)
        (1000, Vector("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"))
      else
        (1024, Vector("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))

    def getExponent(curBytes: Long, baseValue: Int, curExponent: Int = 0): Int =
      if (curBytes < baseValue) curExponent
      else {
        val newExponent = 1 + curExponent
        getExponent(curBytes / (baseValue * newExponent), baseValue, newExponent)
      }

    val exponent = getExponent(bytes, baseValue)
    val divisor = Math.pow(baseValue, exponent)
    val unitString = unitStrings(exponent)

    f"${bytes / divisor}%.1f $unitString"
  }

  def humanReadableDuration(duration: Long): String = {
    val d = new Duration(duration)
    PeriodFormat.getDefault.print(d.toPeriod)
  }
}

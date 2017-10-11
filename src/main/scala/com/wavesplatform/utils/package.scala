package com.wavesplatform

import java.io.File
import java.nio.file.Files

import com.google.common.base.Throwables
import com.wavesplatform.state2.{VariablesStorage, Versioned}
import monix.execution.UncaughtExceptionReporter
import org.h2.mvstore.MVStore
import scorex.utils.ScorexLogging

import scala.util.Try

package object utils extends ScorexLogging {

  val UncaughtExceptionsToLogReporter = UncaughtExceptionReporter(exc => log.error(Throwables.getStackTraceAsString(exc)))

  def base58Length(byteArrayLength: Int): Int = math.ceil(math.log(256) / math.log(58) * byteArrayLength).toInt

  def createMVStore(file: Option[File], encryptionKey: Option[Array[Char]] = None): MVStore = {
    val builder = file.fold(new MVStore.Builder) { p =>
      p.getParentFile.mkdirs()

      new MVStore.Builder()
        .fileName(p.getCanonicalPath)
        .autoCommitDisabled()
        .compress()
    }

    try {
      val store = encryptionKey match {
        case Some(key) => builder.encryptionKey(key).open()
        case _ => builder.open()
      }

      store.rollback()
      store
    }
    catch {
      case e: IllegalStateException if e.getMessage.contains("corrupt") =>
        throw new IllegalStateException("wallet.password is incorrect or " + e.getMessage)
    }
  }

  def isVersionValid[A <: VariablesStorage](vs: A)(implicit vr : Versioned[A]): Boolean =
    vs.getInt(vr.versionFieldKey) match {
      case None =>
        vs.putInt(vr.versionFieldKey, vr.codeVersion)
        vs.db.commit()
        true
      case Some(v) => v == vr.codeVersion
    }


  def createWithStore[A <: AutoCloseable with VariablesStorage](storeFile: Option[File], f: => A, pred: A => Boolean = (_: A) => true, deleteExisting: Boolean = false)
                                                                           (implicit vr : Versioned[A]): Try[A] = Try {
    for (fileToDelete <- storeFile if deleteExisting) Files.delete(fileToDelete.toPath)
    val a = f
    if (isVersionValid(a) && pred(a)) a else storeFile match {
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
}



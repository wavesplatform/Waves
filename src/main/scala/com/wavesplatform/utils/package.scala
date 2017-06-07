package com.wavesplatform

import java.io.File
import java.nio.file.Files

import org.h2.mvstore.MVStore
import scorex.utils.ScorexLogging

import scala.util.Try

package object utils extends ScorexLogging {

  def base58Length(byteArrayLength: Int): Int = math.ceil(math.log(256) / math.log(58) * byteArrayLength).toInt

  def createMVStore(file: Option[File], encryptionKey: Option[Array[Char]] = None): MVStore = {
    val builder = file.fold(new MVStore.Builder) { p =>
      p.getParentFile.mkdirs()
      new MVStore.Builder()
        .fileName(p.getCanonicalPath)
        .autoCommitDisabled()
        .compress()
    }

    encryptionKey match {
      case Some(key) => builder.encryptionKey(key).open()
      case _ => builder.open()
    }
  }

  def createWithStore[A <: AutoCloseable](storeFile: Option[File], f: => A, pred: A => Boolean, deleteExisting: Boolean = false): Try[A] = Try {
    for (fileToDelete <- storeFile if deleteExisting) Files.delete(fileToDelete.toPath)
    val a = f
    if (pred(a)) a else storeFile match {
      case Some(file) =>
        log.debug(s"Re-creating file store at $file")
        a.close()
        Files.delete(file.toPath)
        val newA = f
        require(pred(a), "store is inconsistent")
        newA
      case None => throw new IllegalArgumentException("in-memory store is corrupted")
    }
  }
}

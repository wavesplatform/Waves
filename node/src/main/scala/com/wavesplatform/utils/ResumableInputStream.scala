package com.wavesplatform.utils

import java.io.{EOFException, IOException, InputStream}
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/** Reopens the source at the last delivered byte when a read fails, so a long download survives a dropped connection. */
final class ResumableInputStream(
    open: Long => (InputStream, Option[Long]),
    startOffset: Long,
    maxRetries: Int,
    retryDelay: FiniteDuration
) extends InputStream
    with ScorexLogging {
  private var position             = startOffset
  private var current: InputStream = null
  private var end: Option[Long]    = None
  // Closed from the importer's shutdown hook while the main thread may be reading; must stop it from reopening.
  @volatile private var closed = false

  override def read(): Int = {
    val b = new Array[Byte](1)
    if (read(b, 0, 1) < 0) -1 else b(0) & 0xff
  }

  override def read(b: Array[Byte], off: Int, len: Int): Int = if (len == 0) 0 else readWithRetry(b, off, len, failures = 0)

  @tailrec
  private def readWithRetry(b: Array[Byte], off: Int, len: Int, failures: Int): Int =
    Try {
      if (closed) throw new IOException("Stream closed")
      if (current == null) {
        val (in, length) = open(position)
        current = in
        end = length.map(position + _)
      }
      val n = current.read(b, off, len)
      if (n < 0 && end.exists(position < _)) throw new EOFException(s"Stream ended at offset $position, expected ${end.get}")
      n
    } match {
      case Success(n) =>
        if (n > 0) position += n
        n
      case Failure(e: IOException) if !closed && failures < maxRetries =>
        log.warn(s"Read failed at offset $position, reopening: $e")
        closeCurrent()
        Thread.sleep(retryDelay.toMillis)
        readWithRetry(b, off, len, failures + 1)
      case Failure(e) => throw e
    }

  private def closeCurrent(): Unit = {
    if (current != null) Try(current.close())
    current = null
  }

  override def close(): Unit = {
    closed = true
    closeCurrent()
  }
}

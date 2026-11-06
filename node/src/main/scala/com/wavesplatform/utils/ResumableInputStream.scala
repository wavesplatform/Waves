package com.wavesplatform.utils

import java.io.{EOFException, IOException, InputStream}
import java.net.{HttpURLConnection, URL}
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
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

object ResumableInputStream {

  /** Opens `url` from a given offset. The first response fixes the file's validator and size, so a resume cannot splice in a different file. */
  def openHttp(url: URL, timeout: FiniteDuration): Long => (InputStream, Option[Long]) = {
    var validator: Option[String] = None
    var fileSize: Option[Long]    = None

    offset => {
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setConnectTimeout(timeout.toMillis.toInt)
      connection.setReadTimeout(timeout.toMillis.toInt)
      if (offset > 0) {
        connection.setRequestProperty("Range", s"bytes=$offset-")
        validator.foreach(connection.setRequestProperty("If-Range", _))
      }
      val status = connection.getResponseCode
      // A client error other than a timeout or throttling will not go away on retry.
      if (status >= 400 && status < 500 && status != 408 && status != 429) throw new IllegalStateException(s"$url: HTTP $status")
      val in = connection.getInputStream
      try {
        val contentRange = Option(connection.getHeaderField("Content-Range"))
        // A server that ignores Range, or whose file no longer matches If-Range, sends the file from the start.
        val responseValidator = strongValidator(connection)
        if (offset > 0 && (status != HttpURLConnection.HTTP_PARTIAL || !contentRange.exists(_.startsWith(s"bytes $offset-"))))
          throw new IllegalStateException(
            if (validator.isDefined && responseValidator != validator)
              s"$url changed since the download started (${validator.get} -> ${responseValidator.getOrElse("none")})"
            else s"$url did not honour Range: bytes=$offset- (status $status, Content-Range ${contentRange.getOrElse("absent")})"
          )

        val length = Some(connection.getContentLengthLong).filter(_ >= 0)
        val size   = if (offset > 0) contentRange.flatMap(_.split('/').lastOption).flatMap(_.toLongOption) else length
        (fileSize, size) match {
          case (Some(expected), Some(actual)) if expected != actual => throw new IllegalStateException(s"$url changed size from $expected to $actual bytes")
          case _                                                    => if (fileSize.isEmpty) fileSize = size
        }
        if (validator.isEmpty) validator = responseValidator

        in -> length
      } catch {
        case NonFatal(e) =>
          in.close()
          throw e
      }
    }
  }

  // If-Range only accepts a strong validator.
  private def strongValidator(connection: HttpURLConnection): Option[String] =
    Option(connection.getHeaderField("ETag")).filterNot(_.startsWith("W/")).orElse(Option(connection.getHeaderField("Last-Modified")))
}

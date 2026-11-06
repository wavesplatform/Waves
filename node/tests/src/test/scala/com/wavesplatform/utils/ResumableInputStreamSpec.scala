package com.wavesplatform.utils

import com.google.common.io.ByteStreams
import com.wavesplatform.test.FreeSpec

import java.io.{ByteArrayInputStream, IOException, InputStream}
import scala.collection.mutable
import scala.concurrent.duration.*

class ResumableInputStreamSpec extends FreeSpec {
  private val data = Array.tabulate[Byte](1000)(_.toByte)

  private enum Cut {
    case Fail, Truncate
  }

  /** Serves `data` from the requested offset, announcing the remaining length; the first `cuts` opens deliver `cutAfter` bytes and then stop. */
  private class Source(cut: Cut, cutAfter: Int = 300, cuts: Int = 1) {
    val opened = mutable.Buffer.empty[Long]

    def open(offset: Long): (InputStream, Option[Long]) = {
      val remaining = data.drop(offset.toInt)
      val stream =
        if (opened.size >= cuts) new ByteArrayInputStream(remaining)
        else {
          val head = new ByteArrayInputStream(remaining.take(cutAfter))
          cut match {
            case Cut.Truncate => head
            case Cut.Fail =>
              new InputStream {
                override def read(): Int = {
                  val b = head.read()
                  if (b < 0) throw new IOException("Connection reset") else b
                }
              }
          }
        }
      opened += offset
      stream -> Some(remaining.length.toLong)
    }
  }

  private def resumable(source: Source, maxRetries: Int = 3) =
    new ResumableInputStream(source.open, startOffset = 0, maxRetries, retryDelay = Duration.Zero)

  "resumes from the last delivered byte after a read failure" in {
    val source = new Source(Cut.Fail)

    ByteStreams.toByteArray(resumable(source)) shouldBe data
    source.opened shouldBe Seq(0L, 300L)
  }

  "resumes when the source ends before its announced length" in {
    val source = new Source(Cut.Truncate)

    ByteStreams.toByteArray(resumable(source)) shouldBe data
    source.opened shouldBe Seq(0L, 300L)
  }

  "gives up after maxRetries consecutive failures that deliver nothing" in {
    val source = new Source(Cut.Fail, cutAfter = 0, cuts = Int.MaxValue)

    an[IOException] should be thrownBy ByteStreams.toByteArray(resumable(source, maxRetries = 2))
    source.opened shouldBe Seq(0L, 0L, 0L)
  }

  "counts only consecutive failures, so a source that keeps making progress is read to the end" in {
    val source = new Source(Cut.Fail, cutAfter = 100, cuts = 9)

    ByteStreams.toByteArray(resumable(source, maxRetries = 1)) shouldBe data
    source.opened shouldBe (0L to 900L by 100L)
  }

  "does not reopen the source once closed" in {
    val source = new Source(Cut.Fail)
    val in     = resumable(source)
    in.read() shouldBe 0

    in.close()

    an[IOException] should be thrownBy in.read()
    source.opened shouldBe Seq(0L)
  }
}

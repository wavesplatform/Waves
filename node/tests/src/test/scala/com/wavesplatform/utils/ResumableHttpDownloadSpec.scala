package com.wavesplatform.utils

import com.google.common.io.ByteStreams
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import com.wavesplatform.test.FreeSpec

import java.net.{InetSocketAddress, URI, URL}
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.Using

class ResumableHttpDownloadSpec extends FreeSpec {
  private val data = Array.tabulate[Byte](100_000)(_.toByte)

  /** Serves `data`, honouring `Range: bytes=N-`; `respond` may override the response for the n-th request. */
  private def withServer(respond: PartialFunction[(Int, HttpExchange, Long), Unit])(f: (URL, mutable.Buffer[Option[String]]) => Unit): Unit = {
    val ranges = mutable.Buffer.empty[Option[String]]
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext(
      "/blockchain",
      { exchange =>
        val range  = Option(exchange.getRequestHeaders.getFirst("Range"))
        val offset = range.fold(0L)(_.stripPrefix("bytes=").stripSuffix("-").toLong)
        val n      = ranges.synchronized { ranges += range; ranges.size - 1 }
        respond.applyOrElse((n, exchange, offset), (_, e, o) => serve(e, o))
        exchange.close()
      }
    )
    // A stalled handler must not hold up the resumed request.
    val executor = Executors.newCachedThreadPool()
    server.setExecutor(executor)
    server.start()
    try f(URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/blockchain").toURL, ranges)
    finally {
      server.stop(0)
      executor.shutdownNow()
    }
  }

  private def serve(exchange: HttpExchange, offset: Long, limit: Int = Int.MaxValue): Unit = {
    val remaining = data.drop(offset.toInt)
    if (offset > 0) exchange.getResponseHeaders.set("Content-Range", s"bytes $offset-${data.length - 1}/${data.length}")
    exchange.sendResponseHeaders(if (offset > 0) 206 else 200, remaining.length)
    Using.resource(exchange.getResponseBody)(_.write(remaining.take(limit)))
  }

  private def download(url: URL, timeout: FiniteDuration = 5.seconds) =
    ByteStreams.toByteArray(new ResumableInputStream(ResumableInputStream.openHttp(url, timeout), startOffset = 0, maxRetries = 3, Duration.Zero))

  "resumes a download whose connection drops mid-body with a Range request" in withServer { case (0, exchange, offset) =>
    serve(exchange, offset, limit = 30_000)
  } { (url, ranges) =>
    download(url) shouldBe data
    ranges shouldBe Seq(None, Some("bytes=30000-"))
  }

  "fails without retrying when the server answers a Range request with the whole file" in withServer {
    case (0, exchange, offset) => serve(exchange, offset, limit = 30_000)
    case (_, exchange, _)      => serve(exchange, offset = 0)
  } { (url, ranges) =>
    val e = the[IllegalStateException] thrownBy download(url)
    e.getMessage should include("bytes=30000-")
    ranges.size shouldBe 2
  }

  "resumes a download whose connection stalls longer than the read timeout" in withServer { case (0, exchange, _) =>
    exchange.sendResponseHeaders(200, data.length)
    val body = exchange.getResponseBody
    body.write(data.take(30_000))
    body.flush()
    Thread.sleep(30_000)
  } { (url, ranges) =>
    val start = System.nanoTime()
    download(url, timeout = 300.millis) shouldBe data
    (System.nanoTime() - start).nanos should be < 10.seconds
    ranges shouldBe Seq(None, Some("bytes=30000-"))
  }
}

package com.wavesplatform.utils

import com.google.common.io.ByteStreams
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import com.wavesplatform.test.FreeSpec

import java.net.{InetSocketAddress, URI, URL}
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Using

class ResumableHttpDownloadSpec extends FreeSpec {
  private case class File(bytes: Array[Byte], etag: Option[String])
  private case class Request(range: Option[String], ifRange: Option[String])

  private val data = Array.tabulate[Byte](100_000)(_.toByte)
  private val v1   = File(data, Some("\"v1\""))

  /** Serves `v1`, honouring `Range: bytes=N-` and `If-Range`; `respond` may override the response for the n-th request. */
  private def withServer(respond: PartialFunction[(Int, HttpExchange, Long), Unit])(f: (URL, () => Seq[Request]) => Unit): Unit = {
    val requests = new ConcurrentLinkedQueue[Request]()
    val server   = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext(
      "/blockchain",
      { exchange =>
        val headers = exchange.getRequestHeaders
        val request = Request(Option(headers.getFirst("Range")), Option(headers.getFirst("If-Range")))
        val offset  = request.range.fold(0L)(_.stripPrefix("bytes=").stripSuffix("-").toLong)
        val n = requests.synchronized {
          requests.add(request)
          requests.size - 1
        }
        respond.applyOrElse((n, exchange, offset), (_, e, o) => serve(e, o))
        exchange.close()
      }
    )
    // A stalled handler must not hold up the resumed request.
    val executor = Executors.newCachedThreadPool()
    server.setExecutor(executor)
    server.start()
    try f(URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/blockchain").toURL, () => requests.asScala.toSeq)
    finally {
      server.stop(0)
      executor.shutdownNow()
    }
  }

  private def serve(exchange: HttpExchange, requestedOffset: Long, file: File = v1, limit: Int = Int.MaxValue): Unit = {
    val ifRange = Option(exchange.getRequestHeaders.getFirst("If-Range"))
    // RFC 9110: a validator that no longer matches means "send the whole current file".
    val offset = if (ifRange.exists(v => !file.etag.contains(v))) 0L else requestedOffset
    val remaining = file.bytes.drop(offset.toInt)
    file.etag.foreach(exchange.getResponseHeaders.set("ETag", _))
    if (offset > 0) exchange.getResponseHeaders.set("Content-Range", s"bytes $offset-${file.bytes.length - 1}/${file.bytes.length}")
    exchange.sendResponseHeaders(if (offset > 0) 206 else 200, remaining.length)
    Using.resource(exchange.getResponseBody)(_.write(remaining.take(limit)))
  }

  private def download(url: URL, timeout: FiniteDuration = 5.seconds) =
    ByteStreams.toByteArray(new ResumableInputStream(ResumableInputStream.openHttp(url, timeout), startOffset = 0, maxRetries = 3, Duration.Zero))

  "resumes a download whose connection drops mid-body with a Range request" in withServer { case (0, exchange, offset) =>
    serve(exchange, offset, limit = 30_000)
  } { (url, requests) =>
    download(url) shouldBe data
    requests().map(_.range) shouldBe Seq(None, Some("bytes=30000-"))
  }

  "fails without retrying when the server answers a Range request with the whole file" in withServer {
    case (0, exchange, offset) => serve(exchange, offset, limit = 30_000)
    case (_, exchange, _)      => serve(exchange, requestedOffset = 0)
  } { (url, requests) =>
    val e = the[IllegalStateException] thrownBy download(url)
    e.getMessage should include("bytes=30000-")
    requests().size shouldBe 2
  }

  "resumes a download whose connection stalls longer than the read timeout" in withServer { case (0, exchange, _) =>
    exchange.sendResponseHeaders(200, data.length)
    val body = exchange.getResponseBody
    body.write(data.take(30_000))
    body.flush()
    Thread.sleep(30_000)
  } { (url, requests) =>
    val start = System.nanoTime()
    download(url, timeout = 300.millis) shouldBe data
    (System.nanoTime() - start).nanos should be < 10.seconds
    requests().map(_.range) shouldBe Seq(None, Some("bytes=30000-"))
  }

  "fails without retrying when the file is not found" in withServer { case (_, exchange, _) =>
    exchange.sendResponseHeaders(404, -1)
  } { (url, requests) =>
    the[IllegalStateException] thrownBy download(url) should have message s"$url: HTTP 404"
    requests().size shouldBe 1
  }

  "retries a server error" in withServer { case (0, exchange, _) =>
    exchange.sendResponseHeaders(503, -1)
  } { (url, requests) =>
    download(url) shouldBe data
    requests().size shouldBe 2
  }

  "fails when the file was replaced between connections, using the ETag of the first response" in withServer {
    case (0, exchange, offset) => serve(exchange, offset, limit = 30_000)
    case (_, exchange, offset) => serve(exchange, offset, File(data.reverse, Some("\"v2\"")))
  } { (url, requests) =>
    the[IllegalStateException] thrownBy download(url) should have message s"""$url changed since the download started ("v1" -> "v2")"""
    requests().map(_.ifRange) shouldBe Seq(None, Some("\"v1\""))
  }

  "fails when the file was replaced between connections by one of a different size" in withServer {
    case (0, exchange, offset) => serve(exchange, offset, File(data, None), limit = 30_000)
    case (_, exchange, offset) => serve(exchange, offset, File(data ++ data, None))
  } { (url, _) =>
    the[IllegalStateException] thrownBy download(url) should have message s"$url changed size from 100000 to 200000 bytes"
  }
}

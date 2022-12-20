package com.wavesplatform.grpc.observers

import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import org.slf4j.LoggerFactory

import java.util.concurrent.atomic.AtomicBoolean

class ManualGrpcObserver[RequestT, EventT] extends ClientResponseObserver[RequestT, EventT] with AutoCloseable with ScorexLogging {
  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"${getSimpleName(this)}#${hashCode()}"))

  private val working                                           = new AtomicBoolean(true)
  private var requestStream: ClientCallStreamObserver[RequestT] = null

  override def beforeStart(requestStream: ClientCallStreamObserver[RequestT]): Unit = {
    log.info("Starting...")
    this.requestStream = requestStream
    requestStream.disableAutoRequestWithInitial(1)

    // Works only for bidi-streams or unary calls, because it indicates when a client is ready to _send_ messages.
    // See https://grpc.github.io/grpc-java/javadoc/io/grpc/ClientCall.Listener.html#onReady--
    // requestStream.setOnReadyHandler(() => )
  }

  def requestNext(): Unit = ifWorking { requestStream.request(1) }

  override def close(): Unit = if (working.compareAndSet(true, false)) {
    log.info("Closing by a client...")
    Option(requestStream).foreach(_.cancel("Closed by a client", null))
  }

  override def onNext(value: EventT): Unit = {}
  override def onError(t: Throwable): Unit = {}
  override def onCompleted(): Unit         = {}

  protected def ifWorking(f: => Unit): Unit = if (working.get()) f
}

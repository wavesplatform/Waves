package com.wavesplatform.api.observers

import com.wavesplatform.utils.{LoggerFacade, ScorexLogging}
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import org.slf4j.LoggerFactory

import java.util.concurrent.atomic.AtomicBoolean

class ManualGrpcObserver[RequestT, EventT] extends ClientResponseObserver[RequestT, EventT] with ScorexLogging {
  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getSimpleName}#${hashCode()}"))

  private val working                                           = new AtomicBoolean(false)
  private var requestStream: ClientCallStreamObserver[RequestT] = null

  override def beforeStart(requestStream: ClientCallStreamObserver[RequestT]): Unit = {
    log.info("Starting...")
    working.set(true)
    this.requestStream = requestStream
    requestStream.disableAutoRequestWithInitial(1)
    // Works only for bidi-streams or unary calls, because it indicates when a client is ready to _send_ messages.
    // See https://grpc.github.io/grpc-java/javadoc/io/grpc/ClientCall.Listener.html#onReady--
    // requestStream.setOnReadyHandler(() => )
  }

  def requestNext(): Unit = ifWorking { requestStream.request(1) }

  def close(cause: Throwable): Unit = if (working.compareAndSet(true, false)) {
    log.warn("Closing by a client...", cause)
    Option(requestStream).foreach(_.cancel("Closed by a client", cause))
  }

  override def onNext(value: EventT): Unit = {}
  override def onError(t: Throwable): Unit = working.set(false)
  override def onCompleted(): Unit         = working.set(false)

  protected def ifWorking(f: => Unit): Unit = if (working.get()) f
}

package scorex.network

import scorex.network.message.MessageSpec

import scala.concurrent.duration.Duration
import scala.util.{Success, Failure, Try}

//todo: anti-ddos

trait SubProtocol[Req] {
  val reqSpec: MessageSpec[Req]

  def sendBytes: Array[Byte] => Try[Unit]

  def send(req: Req): Try[Unit] = sendBytes(reqSpec.serializeData(req))
}

trait AnswerableSubProtocol[Req, Rep] extends SubProtocol[Req] {
  val timeout: Duration

  val replySpec: MessageSpec[Rep]

  lazy val messageCode = replySpec.messageCode

  //handler to be defined. Should return Failure if data is semantically incorrect
  def handler: (Rep) => Try[Unit]

  def onResponse(bytes: Array[Byte]) = replySpec.deserializeData(bytes).flatMap { reply =>
    handler(reply)
  }
}


trait SyncSubProtocol[Req, Rep] extends AnswerableSubProtocol[Req, Rep] {
  private var sentOut: Option[Req] = None

  override def send(req: Req): Try[Unit] = sentOut.synchronized(sentOut match {
    case Some(_) =>
      Failure(new Exception("Still waiting for answer"))
    case None =>
      sentOut = Some(req)
      super.send(req)
      Success(Unit)
  })
}

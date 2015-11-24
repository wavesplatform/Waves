package scorex.network

import scorex.network.message.MessageSpec

import scala.concurrent.duration.Duration


trait SubProtocol[Req] {
  val reqSpec: MessageSpec[Req]
}

trait AnswerableSubProtocol[Req, Rep] extends SubProtocol[Req] {
  val timeout: Duration

  val replySpec: MessageSpec[Rep]

  def handler: (Rep) => Unit
}


trait SyncSubProtocol[Req, Rep] extends AnswerableSubProtocol[Req, Rep]
package scorex.network

import scorex.network.message.MessageSpec


trait SubProtocol[Req] {
  val reqSpec: MessageSpec[Req]
}

trait AnswerableSubProtocol[Req, Rep] extends SubProtocol[Req] {
  val replySpec: MessageSpec[Rep]
}


trait SyncSubProtocol[Req, Rep] extends AnswerableSubProtocol[Req, Rep]
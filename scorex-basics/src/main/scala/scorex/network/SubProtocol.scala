package scorex.network

import scorex.network.message.MessageSpec


trait SubProtocol[Req,Rep] {


  val reqSpec: MessageSpec[Req]
  val replySpec: MessageSpec[Rep]


}

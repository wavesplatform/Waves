package scorex.network

import scorex.network.message.MessageSpec

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

//todo: anti-ddos

trait Interaction

trait StatelessInteraction {
}

trait OnewayInteraction[MsgType] extends StatelessInteraction {
  val msgSpec: MessageSpec[MsgType]
}

trait IncomingInteraction[ReqMsg] extends OnewayInteraction[ReqMsg] {
}

trait OutcoimngInteraction[RepMsg] extends OnewayInteraction[RepMsg] {

}

trait StatefulInteraction {

}

trait DuplexInteraction[ReqMsg, RepMsg] extends StatefulInteraction{

  val reqSpec: MessageSpec[ReqMsg]
  val repSpec: MessageSpec[RepMsg]

}
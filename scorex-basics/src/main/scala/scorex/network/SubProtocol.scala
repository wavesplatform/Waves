package scorex.network


import scorex.network.message.MessageSpec

//todo: anti-ddos

trait Interaction

trait StatelessInteraction {
}

trait SimplexInteraction[MsgType] extends StatelessInteraction {
  val msgSpec: MessageSpec[MsgType]
}

trait IncomingInteraction[ReqMsg] extends SimplexInteraction[ReqMsg] {
}

trait OutcomingInteraction[RepMsg] extends SimplexInteraction[RepMsg] {

}

trait StatefulInteraction {

}


trait DuplexInteraction[ReqMsg, RepMsg] extends StatefulInteraction{
  val reqSpec: MessageSpec[ReqMsg]
  val repSpec: MessageSpec[RepMsg]

}
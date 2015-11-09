package scorex.lagonaki.network.message

case object GetPeersMessage extends Message {
  override val messageType = Message.GetPeersType
  override val dataBytes = Array[Byte]()
}

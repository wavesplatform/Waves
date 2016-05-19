package scorex.serialization

trait BytesSerializable extends Serializable {

  def bytes: Array[Byte]
}

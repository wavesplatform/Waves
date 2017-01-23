package scorex.transaction

import scorex.serialization.BytesSerializable

/**
  * reason to change account balance
  */
trait StateChangeReason extends BytesSerializable {

  val id: Array[Byte]
}

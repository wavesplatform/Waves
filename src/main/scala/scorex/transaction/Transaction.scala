package scorex.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.state2.ByteArray
import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.TransactionParser.TransactionType

trait Transaction extends BytesSerializable with JsonSerializable {
  val id: ByteArray

  val transactionType: TransactionType.Value
  val assetFee: (Option[AssetId], Long)
  val timestamp: Long
  override def toString: String = json.toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id == tx.id
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(id.arr.takeRight(4))

}

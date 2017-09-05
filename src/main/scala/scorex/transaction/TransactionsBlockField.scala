package scorex.transaction

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.network.TransactionMessageSpec
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.block.{Block, BlockField}

trait TransactionsBlockField extends BlockField[Seq[Transaction]]

object TransactionsBlockField {
  def apply(version: Int, value: Seq[Transaction]): TransactionsBlockField = version match {
    case 1 | 2 => TransactionsBlockFieldVersion1or2(value)
    case 3 => TransactionsBlockFieldVersion3(value)
  }

  def serTxs(value: Seq[Transaction], serTxCount: Array[Byte]): Array[Byte] = {
    val byteBuffer = ByteBuffer.allocate(2 + value.size * TransactionMessageSpec.maxLength)
    byteBuffer.put(serTxCount)
    value.foreach { tx =>
      val txBytes = tx.bytes
      byteBuffer.put(Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0))
      byteBuffer.put(txBytes)
    }
    byteBuffer.array()
  }
}

case class TransactionsBlockFieldVersion1or2(override val value: Seq[Transaction]) extends TransactionsBlockField {
  override val name = "transactions"

  override lazy val json: JsObject = Json.obj(name -> JsArray(value.map(_.json)))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= Block.MaxTransactionsPerBlockVer1Ver2).toByte
    TransactionsBlockField.serTxs(value, Array(txCount))
  }
}

case class TransactionsBlockFieldVersion3(override val value: Seq[Transaction]) extends TransactionsBlockField {
  override val name = "transactions"

  override lazy val json: JsObject = Json.obj(name -> JsArray(value.map(_.json)))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= Block.MaxTransactionsPerBlockVer3)
    // https://stackoverflow.com/a/18247942/288091
    val size0 = (txCount & 0xFF).asInstanceOf[Byte]
    val size1 = ((txCount >> 8) & 0xFF).asInstanceOf[Byte]
    val serTxCount = Seq(size0, size1).toArray
    TransactionsBlockField.serTxs(value, serTxCount)
  }
}

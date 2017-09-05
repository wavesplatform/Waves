package scorex.transaction

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.network.TransactionMessageSpec
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.block.{Block, BlockField}

trait TransactionsBlockField extends BlockField[Seq[Transaction]]

object TransactionsBlockField {
  def apply(version: Int, value: Seq[Transaction]): TransactionsBlockField = version match {
    case 1 | 2 => TransactionsBlockFieldVersion1or2(value)
    case 3 => TransactionsBlockFieldVersion3(value)
  }

  def serTxs(value: Seq[Transaction], serTxCount: Array[Byte]): Array[Byte] = {
    val byteBuffer = new ByteArrayOutputStream(value.size * TransactionMessageSpec.maxLength / 2)
    byteBuffer.write(serTxCount, 0, serTxCount.length)
    value.foreach { tx =>
      val txBytes = tx.bytes
      val txSize = Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0)
      byteBuffer.write(txSize, 0, txSize.length)
      byteBuffer.write(txBytes, 0, txBytes.length)
    }
    byteBuffer.toByteArray()
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

package scorex.transaction

import com.google.common.primitives.{Bytes, Ints}
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.block.{Block, BlockField}

trait TransactionsBlockField extends BlockField[Seq[Transaction]]

object TransactionsBlockField {
  def apply(version: Int, value: Seq[Transaction]): TransactionsBlockField = version match {
    case 1 => TransactionsBlockFieldVersion1(value)
    case 2 => TransactionsBlockFieldVersion2(value)
  }
}

case class TransactionsBlockFieldVersion1(override val value: Seq[Transaction]) extends TransactionsBlockField {
  override val name = "transactions v1"

  override lazy val json: JsObject = Json.obj(name -> JsArray(value.map(_.json)))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= Block.MaxTransactionsPerBlockVer1).toByte
    value.foldLeft(Array(txCount)) { case (bs, tx) =>
      val txBytes = tx.bytes
      bs ++ Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0) ++ txBytes
    }
  }
}

case class TransactionsBlockFieldVersion2(override val value: Seq[Transaction]) extends TransactionsBlockField {
  override val name = "transactions v1"

  override lazy val json: JsObject = Json.obj(name -> JsArray(value.map(_.json)))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= Block.MaxTransactionsPerBlockVer2)
    val serTxCount = Seq((txCount / 256).toByte, txCount % 256).map(_.toByte).toArray
    value.foldLeft(serTxCount) { case (bs, tx) =>
      val txBytes = tx.bytes
      bs ++ Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0) ++ txBytes
    }
  }
}

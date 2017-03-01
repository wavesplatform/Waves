package scorex.transaction

import com.google.common.primitives.{Bytes, Ints}
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.block.BlockField

case class TransactionsBlockField(override val value: Seq[Transaction])
  extends BlockField[Seq[Transaction]] {

  import SimpleTransactionModule.MaxTransactionsPerBlock

  override val name = "transactions"

  override lazy val json: JsObject = Json.obj(name -> JsArray(value.map(_.json)))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= MaxTransactionsPerBlock).toByte
    value.foldLeft(Array(txCount)) { case (bs, tx) =>
      val txBytes = tx.bytes
      bs ++ Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0) ++ txBytes
    }
  }
}

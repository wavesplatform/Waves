package scorex.transaction.Data

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.encode.Base58
import scorex.serialization.BytesSerializable
import scorex.transaction._
import scorex.transaction.TransactionParser._

/**
  * Created by DN on 30/05/2017.
  */
sealed trait DataTransaction extends SignedTransaction
{
  def data: Array[Byte]
}

object DataTransaction
{
  private case class  DataTransactionImpl(sender: PublicKeyAccount,
                                          data: Array[Byte],
                                          feeAssetId: Option[AssetId],
                                          fee: Long,
                                          timestamp:Long,
                                          signature: Array[Byte])
  extends DataTransaction
  {
    override val transactionType: TransactionType.Value = TransactionType.DataTransaction
    override val assetFee: (Option[AssetId], Long)      = (None, fee)

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
                                                sender.publicKey,
                                                BytesSerializable.arrayWithSize(data),
                                                Longs.toByteArray(fee),
                                                Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "data" -> Base58.encode(data)
    )

    override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)
  }




}

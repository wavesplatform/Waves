package com.wavesplatform.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.serialization.impl.GenesisTxSerializer
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

case class GenesisTransaction private (recipient: Address, amount: Long, timestamp: Long, signature: ByteStr) extends Transaction {
  override val builder                 = GenesisTransaction
  override val assetFee: (Asset, Long) = (Waves, 0)
  override val id: Coeval[ByteStr]     = Coeval.evalOnce(signature)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val bytes: Coeval[Array[Byte]]     = bodyBytes
  override val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))
}

object GenesisTransaction extends TransactionParserLite {
  override type TransactionT = GenesisTransaction

  override val typeId: TxType                    = 1
  override val supportedVersions: Set[TxVersion] = Set(1)
  override val classTag                          = ClassTag(classOf[GenesisTransaction])

  val serializer = GenesisTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[GenesisTransaction] =
    serializer.parseBytes(bytes)

  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {
    val typeBytes      = Ints.toByteArray(typeId) // ???
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes    = Longs.toByteArray(amount)
    val amountFill     = new Array[Byte](Longs.BYTES - amountBytes.length)

    val payload = Bytes.concat(typeBytes, timestampBytes, recipient.bytes.arr, Bytes.concat(amountFill, amountBytes))
    val hash    = crypto.fastHash(payload)
    Bytes.concat(hash, hash)
  }

  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    if (amount < 0) {
      Left(TxValidationError.NegativeAmount(amount, "waves"))
    } else {
      val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))
      Right(GenesisTransaction(recipient, amount, timestamp, signature))
    }
  }
}

package scorex.block

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.block.Block.BlockId
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser.SignatureLength
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{Signed, Transaction, TransactionsBlockField, ValidationError}

case class MicroBlock private(version: Byte, generator: PublicKeyAccount, transactionData: Seq[Transaction], prevResBlockSig: BlockId,
                              totalResBlockSig: BlockId, signature: ByteStr) extends Signed {

  private lazy val versionField: ByteBlockField = ByteBlockField("version", version)
  private lazy val prevResBlockSigField: BlockIdField = BlockIdField("prevResBlockSig", prevResBlockSig.arr)
  private lazy val totalResBlockSigField: BlockIdField = BlockIdField("totalResBlockSigField", totalResBlockSig.arr)
  private lazy val signerDataField: SignerDataBlockField = SignerDataBlockField("signature", SignerData(generator, signature))
  private lazy val transactionDataField = TransactionsBlockField(transactionData)

  lazy val uniqueId: ByteStr = signature
  lazy val encodedId: String = uniqueId.base58

  lazy val json: JsObject =
    versionField.json ++
      prevResBlockSigField.json ++
      totalResBlockSigField.json ++
      transactionDataField.json ++
      signerDataField.json ++
      Json.obj(
        "blocksize" -> bytes.length
      )

  lazy val bytes: Array[Byte] = {
    val txBytesSize = transactionDataField.bytes.length
    val txBytes = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionDataField.bytes

    versionField.bytes ++
      prevResBlockSigField.bytes ++
      totalResBlockSigField.bytes ++
      txBytes ++
      signerDataField.bytes
  }

  lazy val bytesWithoutSignature: Array[Byte] = bytes.dropRight(SignatureLength)

  override lazy val signatureValid: Boolean = EllipticCurveImpl.verify(signature.arr, bytesWithoutSignature, generator.publicKey)
  override lazy val signedDescendants: Seq[Signed] = transactionData
}

object MicroBlock {
  def apply(generator: PublicKeyAccount, transactionData: Seq[Transaction], prevResBlockSig: BlockId,
            totalResBlockSig: BlockId, signature: ByteStr): Either[ValidationError, MicroBlock] = {
    if (transactionData.isEmpty)
      Left(GenericError("cannot create empty MicroBlock"))
    else
      Right(new MicroBlock(version = 1: Byte, generator, transactionData, prevResBlockSig, totalResBlockSig, signature))
  }
}


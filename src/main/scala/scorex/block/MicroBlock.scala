package scorex.block

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.state2._
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block.{BlockId, transParseBytes}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser.SignatureLength
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.util.{Failure, Try}

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

object MicroBlock extends ScorexLogging {
  private def create(version: Byte, generator: PublicKeyAccount, transactionData: Seq[Transaction], prevResBlockSig: BlockId,
                     totalResBlockSig: BlockId, signature: ByteStr): Either[ValidationError, MicroBlock] = {
    if (transactionData.isEmpty)
      Left(GenericError("cannot create empty MicroBlock"))
    else
      Right(new MicroBlock(version, generator, transactionData, prevResBlockSig, totalResBlockSig, signature))
  }

  def buildAndSign(generator: PrivateKeyAccount, transactionData: Seq[Transaction], prevResBlockSig: BlockId,
                   totalResBlockSig: BlockId): Either[ValidationError, MicroBlock] = {
    require(prevResBlockSig.arr.length == SignatureLength, "Incorrect prevResBlockSig")
    require(totalResBlockSig.arr.length == SignatureLength, "Incorrect totalResBlockSig")
    require(generator.publicKey.length == TransactionParser.KeyLength, "Incorrect generator.publicKey")

    create(version = 1: Byte, generator, transactionData, prevResBlockSig, totalResBlockSig, ByteStr.empty).map { nonSignedBlock =>
      val toSign = nonSignedBlock.bytes
      val signature = EllipticCurveImpl.sign(generator, toSign)
      nonSignedBlock.copy(signature = ByteStr(signature))
    }
  }

  def parseBytes(bytes: Array[Byte]): Try[MicroBlock] = Try {

    val version = bytes.head

    var position = 1

    val prevResBlockSig = ByteStr(bytes.slice(position, position + SignatureLength))
    position += SignatureLength

    val totalResBlockSig = ByteStr(bytes.slice(position, position + SignatureLength))
    position += SignatureLength

    val tBytesLength = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4
    val tBytes = bytes.slice(position, position + tBytesLength)
    val txBlockField = transParseBytes(tBytes).get
    position += tBytesLength

    val genPK = bytes.slice(position, position + TransactionParser.KeyLength)
    position += TransactionParser.KeyLength

    val signature = ByteStr(bytes.slice(position, position + SignatureLength))

    create(version, PublicKeyAccount(genPK), txBlockField, prevResBlockSig, totalResBlockSig, signature).explicitGet()
  }.recoverWith { case t: Throwable =>
    log.error("Error when parsing block", t)
    Failure(t)
  }

}


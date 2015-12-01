package scorex.perma.consensus

import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json._
import scorex.block.BlockField
import scorex.crypto.{Sha256, EllipticCurveImpl}
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.perma.settings.Constants

import scala.annotation.tailrec

case class PermaConsensusBlockField(override val value: PermaLikeConsensusBlockData)
  extends BlockField[PermaLikeConsensusBlockData] {

  import PermaConsensusBlockField._

  override val name: String = PermaConsensusBlockField.fieldName

  override def bytes: Array[Byte] =
    Bytes.ensureCapacity(Ints.toByteArray(value.target.toByteArray.length), 4, 0) ++ value.target.toByteArray ++
      Bytes.ensureCapacity(value.puz, PuzLength, 0) ++
      Bytes.ensureCapacity(value.ticket.publicKey, PublicKeyLength, 0) ++
      Bytes.ensureCapacity(value.ticket.s, SLength, 0) ++
      Bytes.ensureCapacity(Ints.toByteArray(value.ticket.proofs.length), 4, 0) ++
      value.ticket.proofs.foldLeft(Array.empty: Array[Byte]) { (b, p) =>
        val proofBytes =
          Bytes.ensureCapacity(p.signature, SignatureLength, 0) ++
            Bytes.ensureCapacity(Longs.toByteArray(p.segmentIndex), 8, 0) ++
            Bytes.ensureCapacity(p.segment.data, Constants.segmentSize, 0) ++
            Bytes.ensureCapacity(Ints.toByteArray(p.segment.merklePath.length), 4, 0) ++
            p.segment.merklePath.foldLeft(Array.empty: Array[Byte]) { (acc, d) =>
              acc ++ d
            }
        b ++ proofBytes
      }

  override def json: JsObject = Json.obj(name -> Json.toJson(value))
}

object PermaConsensusBlockField {

  val fieldName: String = "perma-consensus"
  val PuzLength = 32
  val PublicKeyLength = EllipticCurveImpl.KeyLength
  val SLength = 32
  val HashLength = Sha256.DigestSize
  val SignatureLength = EllipticCurveImpl.SignatureLength

  def parse(bytes: Array[Byte]): PermaConsensusBlockField = {
    @tailrec
    def parseProofs(from: Int, total: Int, current: Int, acc: IndexedSeq[PartialProof]): IndexedSeq[PartialProof] = {
      if (current < total) {
        val proofsStart = from
        val signatureStart = proofsStart + SignatureLength
        val dataStart = signatureStart + 8
        val merklePathStart = dataStart + Constants.segmentSize

        val signature = bytes.slice(proofsStart, proofsStart + SignatureLength)
        val signatureIndex = Longs.fromByteArray(bytes.slice(signatureStart, signatureStart + 8))
        val blockData = bytes.slice(dataStart, dataStart + Constants.segmentSize)
        val merklePathSize = Ints.fromByteArray(bytes.slice(merklePathStart, merklePathStart + 4))
        val merklePath = (0 until merklePathSize).map { i =>
          bytes.slice(merklePathStart + 4 + i * HashLength, merklePathStart + 4 + (i + 1) * HashLength)
        }
        parseProofs(
          merklePathStart + 4 + merklePathSize * HashLength,
          total,
          current + 1,
          PartialProof(signature, signatureIndex, AuthDataBlock(blockData, merklePath)) +: acc
        )
      } else {
        acc.reverse
      }
    }

    val targetSize = Ints.fromByteArray(bytes.take(4))
    val targetLength = 4 + targetSize
    val proofsSize = Ints.fromByteArray(bytes.slice(
      PuzLength + targetLength + PublicKeyLength + SLength, PuzLength + targetLength + PublicKeyLength + SLength + 4))

    PermaConsensusBlockField(PermaLikeConsensusBlockData(
      BigInt(bytes.slice(4, targetLength)),
      bytes.slice(targetLength, PuzLength + targetLength),
      Ticket(
        bytes.slice(PuzLength + targetLength, PuzLength + targetLength + PublicKeyLength),
        bytes.slice(PuzLength + targetLength + PublicKeyLength, PuzLength + targetLength + PublicKeyLength + SLength),
        parseProofs(PuzLength + targetLength + PublicKeyLength + SLength + 4, proofsSize, 0, IndexedSeq.empty)
      )
    ))
  }
}

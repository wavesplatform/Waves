package scorex.transaction

import com.wavesplatform.state2._
import monix.eval.Coeval
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.ValidationError.GenericError

import scala.util.Try

case class Proofs private(proofs: Seq[ByteStr]) {
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Proofs.Version +: Deser.serializeArrays(proofs.map(_.arr)))
  val base58: Coeval[Seq[String]] = Coeval.evalOnce(proofs.map(p => Base58.encode(p.arr)))
}

object Proofs {

  val Version = 1: Byte
  val MaxProofs = 8
  val MaxProofSize = 512

  lazy val empty = create(Seq.empty).explicitGet()

  def create(proofs: Seq[ByteStr]): Either[ValidationError, Proofs] = for {
    _ <- Either.cond(proofs.lengthCompare(MaxProofs) <= 0, (), GenericError(s"Too many proofs, max $MaxProofs proofs"))
    _ <- Either.cond(!proofs.map(_.arr.length).exists(_ > MaxProofSize), (), GenericError(s"Too large proof, must be max $MaxProofSize bytes"))
  } yield Proofs(proofs)

  def fromBytes(ab: Array[Byte]): Either[ValidationError, Proofs] = for {
    _ <- Either.cond(ab.headOption contains 1, (), GenericError("Proofs version must be 1"))
    arrs <- Try(Deser.parseArrays(ab.tail)).toEither.left.map(er => GenericError(er.toString))
    r <- create(arrs.map(ByteStr(_)))
  } yield r
}
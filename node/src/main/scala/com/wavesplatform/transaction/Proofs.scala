package com.wavesplatform.transaction

import com.google.common.primitives.Bytes
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval

import scala.util.Try
import com.wavesplatform.transaction.TxValidationError.UsupportedProofVersion
import com.wavesplatform.transaction.TxValidationError.TooManyProofs
import com.wavesplatform.transaction.TxValidationError.ToBigProof

case class Proofs(proofs: List[ByteStr]) {
  val bytes: Coeval[Array[Byte]]  = Coeval.evalOnce(Bytes.concat(Array(Proofs.Version), Deser.serializeArrays(proofs.map(_.arr))))
  val base58: Coeval[Seq[String]] = Coeval.evalOnce(proofs.map(p => Base58.encode(p.arr)))
  def toSignature: ByteStr        = proofs.headOption.getOrElse(ByteStr.empty)
  override def toString: String   = s"Proofs(${proofs.mkString(", ")})"
}

object Proofs {
  val Version            = 1: Byte
  val MaxProofs          = 8
  val MaxProofSize       = 64
  val MaxProofStringSize = base58Length(MaxProofSize)

  lazy val empty = new Proofs(Nil)

  protected def validate(proofs: Seq[ByteStr]): Either[ValidationError, Unit] = {
    for {
      _ <- Either.cond(proofs.lengthCompare(MaxProofs) <= 0, (), TooManyProofs(MaxProofs, proofs.length))
      biggestProofSize = if (proofs.nonEmpty) proofs.map(_.arr.length).max else 0
      _ <- Either.cond(biggestProofSize <= MaxProofSize, (), ToBigProof(MaxProofSize, biggestProofSize))
    } yield ()
  }

  def createWithBytes(proofs: Seq[ByteStr], parsedBytes: Array[Byte]): Either[ValidationError, Proofs] =
    validate(proofs) map { _ =>
      new Proofs(proofs.toList) {
        override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
          val proofsLength = 3 + proofs.map(_.length + 2).sum
          if (parsedBytes.length == proofsLength) parsedBytes else parsedBytes.take(proofsLength)
        }
      }
    }

  def create(proofs: Seq[ByteStr]): Either[ValidationError, Proofs] =
    validate(proofs).map(_ => Proofs(proofs.toList))

  def fromBytes(ab: Array[Byte]): Either[ValidationError, Proofs] =
    for {
      version <- Try(ab.head.toInt).toEither.left.map(err => GenericError(err.toString))
      _    <- Either.cond(version == 1, (), UsupportedProofVersion(version, List(1)))
      arrs <- Try(Deser.parseArrays(ab.tail)).toEither.left.map(er => GenericError(er.toString))
      r    <- createWithBytes(arrs.map(ByteStr(_)), ab)
    } yield r

  implicit def apply(proofs: Seq[ByteStr]): Proofs = new Proofs(proofs.toList)
  implicit def toSeq(proofs: Proofs): Seq[ByteStr] = proofs.proofs
}

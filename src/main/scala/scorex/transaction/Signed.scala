package scorex.transaction

import scorex.crypto.EllipticCurveImpl
import scorex.serialization.BytesSerializable
import scorex.transaction.ValidationResult.ValidationResult

trait Verifiable extends BytesSerializable {
  def publicKey: Array[Byte]
}

case class View[A](value: A, signature: Array[Byte])

sealed trait Signed[A] {
  def value: A

  def signature: Array[Byte]
}

object Signed {

  private case class SignedImpl[A](value: A, signature: Array[Byte]) extends Signed[A]

  def sign[A <: BytesSerializable](value: A, privateKey: Array[Byte]): Signed[A] = SignedImpl(value, EllipticCurveImpl.sign(privateKey, value.bytes))

  def verify[A <: Verifiable](view: View[A]): Either[ValidationResult, Signed[A]] = {
    if (!EllipticCurveImpl.verify(view.signature, view.value.bytes, view.value.publicKey)) {
      Left(ValidationResult.InvalidSignature)
    } else {
      Right(SignedImpl(view.value, view.signature))
    }
  }
}
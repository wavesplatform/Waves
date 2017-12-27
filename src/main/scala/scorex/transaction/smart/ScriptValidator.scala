package scorex.transaction.smart

import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.InvalidProof
import scorex.transaction.{ProvenTransaction, Transaction, ValidationError}

object ScriptValidator {
  def verify[T <: Transaction](script: Script, transaction: T): Either[ValidationError, T] = Right(transaction)

  def verifyEllipticCurveSignature[TP <: ProvenTransaction](transaction: TP): Either[ValidationError, TP] = transaction match {
    case sc: SetScriptTransaction => Either.cond(EllipticCurveImpl.verify(sc.proof.arr, sc.toSign(), sc.sender.publicKey), transaction, InvalidProof(sc))
  }
}

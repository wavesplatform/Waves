package scorex.transaction.smart

import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.{GenericError, InvalidProof}
import scorex.transaction.smart.lang.Terms
import scorex.transaction.smart.lang.Terms.Context
import scorex.transaction.{ProvenTransaction, Transaction, ValidationError}

object ScriptValidator {
  def verify[T <: Transaction](script: Script, height:Int, transaction: T): Either[ValidationError, T] = Terms.eval(Context(height, transaction), script.script) match {
    case Left(execError) => Left(GenericError("script execution error"))
    case Right(false) => Left(GenericError("tx not allowed because script"))
    case Right(true) => Right(transaction)
  }

  def verifyAsEllipticCurveSignature[TP <: ProvenTransaction](transaction: TP): Either[ValidationError, TP] = transaction match {
    case sc: SetScriptTransaction => Either.cond(EllipticCurveImpl.verify(sc.proof.arr, sc.bodyBytes(), sc.sender.publicKey), transaction, InvalidProof(sc))
  }
}

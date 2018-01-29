package scorex.transaction.smart

import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.{GenericError, TransactionNotAllowedByScript}
import scorex.transaction._
import scorex.transaction.smart.lang.Evaluator
import scorex.transaction.smart.lang.Evaluator.Context

object Verifier {

  def apply(s: SnapshotStateReader, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] = tx match {
    case _: GenesisTransaction => Right(tx)
    case pt: ProvenTransaction => (pt, s.accountScript(pt.sender)) match {
      case (_, Some(script)) => verify(script, currentBlockHeight, pt)
      case (stx: SignedTransaction, None) => stx.signaturesValid()
      case _ => verifyAsEllipticCurveSignature(pt)
    }
  }

  def verify[T <: ProvenTransaction](script: Script, height: Int, transaction: T): Either[ValidationError, T] = Evaluator.apply[Boolean](Context(height, transaction, Map.empty), script.script) match {
    case Left(execError) => Left(GenericError(s"Script execution error: $execError"))
    case Right(false) => Left(TransactionNotAllowedByScript(transaction))
    case Right(true) => Right(transaction)
  }

  def verifyAsEllipticCurveSignature[T <: ProvenTransaction](pt: T): Either[ValidationError, T] =
    Either.cond(EllipticCurveImpl.verify(pt.proofs.proofs(0).arr, pt.bodyBytes(), pt.sender.publicKey), pt, GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))

}

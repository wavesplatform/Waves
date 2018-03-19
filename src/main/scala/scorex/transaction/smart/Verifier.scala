package scorex.transaction.smart

import com.wavesplatform.crypto
import com.wavesplatform.lang.Evaluator
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.transaction.ValidationError.{GenericError, TransactionNotAllowedByScript}
import scorex.transaction._
import scorex.transaction.assets.{MassTransferTransaction, ScriptTransferTransaction, TransferTransaction}
import cats.syntax.all._

object Verifier {

  def apply(s: SnapshotStateReader, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] =
    (tx match {
      case _: GenesisTransaction => Right(tx)
      case pt: ProvenTransaction =>
        (pt, s.accountScript(pt.sender)) match {
          case (_, Some(script))              => verify(s, script, currentBlockHeight, pt)
          case (stx: SignedTransaction, None) => stx.signaturesValid()
          case _                              => verifyAsEllipticCurveSignature(pt)
        }
    }).flatMap(tx => {
      for {
        assetId <- tx match {
          case t: TransferTransaction       => t.assetId
          case t: ScriptTransferTransaction => t.assetId
          case t: MassTransferTransaction   => t.assetId
          case _                            => None
        }
        script <- s.assetInfo(assetId).flatMap(_.script)
      } yield verify(s, script, currentBlockHeight, tx)
    }.getOrElse(Either.right(tx)))

  def verify[T <: Transaction](s: SnapshotStateReader, script: Script, height: Int, transaction: T): Either[ValidationError, T] = {
    val context = new ConsensusContext(Coeval.evalOnce(transaction), Coeval.evalOnce(height), s).build()
    Evaluator[Boolean](context, script.script) match {
      case Left(execError) => Left(GenericError(s"Script execution error: $execError"))
      case Right(false)    => Left(TransactionNotAllowedByScript(transaction))
      case Right(true)     => Right(transaction)
    }
  }

  def verifyAsEllipticCurveSignature[T <: ProvenTransaction](pt: T): Either[ValidationError, T] =
    Either.cond(
      crypto.verify(pt.proofs.proofs(0).arr, pt.bodyBytes(), pt.sender.publicKey),
      pt,
      GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt")
    )
}

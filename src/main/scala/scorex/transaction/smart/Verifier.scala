package scorex.transaction.smart

import com.wavesplatform.crypto
import com.wavesplatform.lang.Evaluator
import monix.eval.Coeval
import scorex.account.AddressScheme
import scorex.transaction.ValidationError.{GenericError, TransactionNotAllowedByScript}
import scorex.transaction._
import scorex.transaction.assets._
import cats.syntax.all._
import com.wavesplatform.state.Blockchain

object Verifier {

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] =
    (tx match {
      case _: GenesisTransaction => Right(tx)
      case pt: ProvenTransaction =>
        (pt, blockchain.accountScript(pt.sender)) match {
          case (_, Some(script))              => verify(blockchain, script, currentBlockHeight, pt)
          case (stx: SignedTransaction, None) => stx.signaturesValid()
          case _                              => verifyAsEllipticCurveSignature(pt)
        }
    }).flatMap(tx => {
      for {
        assetId <- tx match {
          case t: TransferTransaction          => t.assetId
          case t: VersionedTransferTransaction => t.assetId
          case t: MassTransferTransaction      => t.assetId
          case t: BurnTransaction              => Some(t.assetId)
          case t: ReissueTransaction           => Some(t.assetId)
          case _                               => None
        }

        script <- blockchain.assetDescription(assetId).flatMap(_.script)
      } yield verify(blockchain, script, currentBlockHeight, tx)
    }.getOrElse(Either.right(tx)))

  def verify[T <: Transaction](blockchain: Blockchain, script: Script, height: Int, transaction: T): Either[ValidationError, T] = {
    val context = new BlockchainContext(AddressScheme.current.chainId, Coeval.evalOnce(transaction), Coeval.evalOnce(height), blockchain).build()
    Evaluator[Boolean](context, script.script) match {
      case Left(execError) => Left(GenericError(s"Script execution error: $execError"))
      case Right(false)    => Left(TransactionNotAllowedByScript(transaction))
      case Right(true)     => Right(transaction)
    }
  }

  def verifyAsEllipticCurveSignature[T <: ProvenTransaction](pt: T): Either[ValidationError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender.publicKey),
                    pt,
                    GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

}

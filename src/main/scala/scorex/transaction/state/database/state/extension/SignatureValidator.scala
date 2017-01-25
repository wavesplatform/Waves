package scorex.transaction.state.database.state.extension

import scorex.crypto.EllipticCurveImpl
import scorex.transaction.{GenesisTransaction, PaymentTransaction, SignedTransaction, Transaction}

class SignatureValidator extends StateExtension {


  override def isValid(tx: Transaction, height: Int): Boolean = tx match {
    case tx: SignedTransaction => EllipticCurveImpl.verify(tx.signature, tx.toSign, tx.sender.publicKey)
    case tx: PaymentTransaction =>
      //TODO Payment should be extended from SignedTransaction
      val sigData = PaymentTransaction.signatureData(tx.sender, tx.recipient, tx.amount, tx.fee, tx.timestamp)
      EllipticCurveImpl.verify(tx.signature, sigData, tx.sender.publicKey)
    case tx: GenesisTransaction => true //Genesis transaction is unsigned
    case _ => false
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {}
}
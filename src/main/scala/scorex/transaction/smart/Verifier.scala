package scorex.transaction.smart

import com.wavesplatform.crypto
import com.wavesplatform.lang.Context.{CustomType, LazyVal, Obj}
import com.wavesplatform.lang.{Context, Evaluator}
import com.wavesplatform.lang.Terms._
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.transaction.ValidationError.{GenericError, TransactionNotAllowedByScript}
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

object Verifier {

  def apply(s: SnapshotStateReader, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] = tx match {
    case _: GenesisTransaction => Right(tx)
    case pt: ProvenTransaction =>
      (pt, s.accountScript(pt.sender)) match {
        case (_, Some(script)) => verify(script, currentBlockHeight, pt)
        case (stx: SignedTransaction, None) => stx.signaturesValid()
        case _ => verifyAsEllipticCurveSignature(pt)
      }
  }

  def verify[T <: ProvenTransaction](script: Script, height: Int, transaction: T): Either[ValidationError, T] = {

    val context = Context(Map("Transaction" -> transactionType),
      Map(
        ("H", (INT, height)),
        ("TX", (TYPEREF("Transaction"), transactionObject(transaction)))
      ),
      Map.empty)
    Evaluator[Boolean](context, script.script) match {
      case Left(execError) => Left(GenericError(s"Script execution error: $execError"))
      case Right(false) => Left(TransactionNotAllowedByScript(transaction))
      case Right(true) => Right(transaction)
    }
  }

  def verifyAsEllipticCurveSignature[T <: ProvenTransaction](pt: T): Either[ValidationError, T] =
    Either.cond(
      crypto.verify(pt.proofs.proofs(0).arr, pt.bodyBytes(), pt.sender.publicKey),
      pt,
      GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt")
    )

  val optionByteVector = OPTION(BYTEVECTOR)

  val transactionType = CustomType(
    "Transaction",
    List(
      "TYPE" -> INT,
      "ID" -> BYTEVECTOR,
      "BODYBYTES" -> BYTEVECTOR,
      "SENDERPK" -> BYTEVECTOR,
      "PROOFA" -> BYTEVECTOR,
      "PROOFB" -> BYTEVECTOR,
      "PROOFC" -> BYTEVECTOR,
      "ASSETID" -> optionByteVector
    )
  )

  val thro = Coeval(throw new IllegalArgumentException("transactions is of another type"))

  private def proofBinding(tx: Transaction, x: Int) =
    LazyVal(BYTEVECTOR)(tx match {
      case pt: ProvenTransaction =>
        val proof: ByteVector =
          if (x >= pt.proofs.proofs.size)
            ByteVector.empty
          else ByteVector(pt.proofs.proofs(x).arr)
        Coeval.evalOnce(proof)
      case _ => thro
    })

  private def transactionObject(tx: Transaction) =
    Obj(
      Map(
        "TYPE" -> LazyVal(INT)(Coeval.evalOnce(tx.transactionType.id)),
        "ID" -> LazyVal(BYTEVECTOR)(tx.id.map(_.arr).map(ByteVector(_))),
        "BODYBYTES" -> LazyVal(BYTEVECTOR)(tx match {
          case pt: ProvenTransaction => pt.bodyBytes.map(ByteVector(_))
          case _ => thro
        }),
        "SENDERPK" -> LazyVal(BYTEVECTOR)(tx match {
          case pt: Authorized => Coeval.evalOnce(ByteVector(pt.sender.publicKey))
          case _ => thro
        }),
        "ASSETID" -> LazyVal(optionByteVector)(tx match {
          case tt: TransferTransaction => Coeval.evalOnce(tt.assetId.map(x => ByteVector(x.arr)).asInstanceOf[optionByteVector.Underlying])
          case _ => thro
        }),
        "PROOFA" -> proofBinding(tx, 0),
        "PROOFB" -> proofBinding(tx, 1),
        "PROOFC" -> proofBinding(tx, 2)
      ))

}

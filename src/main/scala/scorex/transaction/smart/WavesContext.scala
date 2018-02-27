package scorex.transaction.smart

import cats.data.EitherT
import com.wavesplatform.crypto
import com.wavesplatform.lang.Context._
import com.wavesplatform.lang.Evaluator.TrampolinedExecResult
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang._
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{Authorized, ProvenTransaction, Transaction}

object WavesContext {

  def build(tx: Coeval[Transaction], height: Coeval[Int]): Context = Context(
    Map("Transaction" -> transactionType),
    Map(
      ("H", (INT, height)),
      ("TX", (TYPEREF("Transaction"), transactionObject(tx)))
    ),
    Map(sigVerify.name -> sigVerify)
  )

  val sigVerify = CustomFunction("SIGVERIFY", BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
    case m :: s :: p :: Nil =>
      val bool = crypto.verify(s.asInstanceOf[ByteVector].toArray, m.asInstanceOf[ByteVector].toArray, p.asInstanceOf[ByteVector].toArray)
      Right(bool)
    case _ => ???
  }


  val optionByteVector = OPTION(BYTEVECTOR)

  val transactionType = CustomType(
    "Transaction",
    List(
      "TYPE"      -> INT,
      "ID"        -> BYTEVECTOR,
      "BODYBYTES" -> BYTEVECTOR,
      "SENDERPK"  -> BYTEVECTOR,
      "PROOFA"    -> BYTEVECTOR,
      "PROOFB"    -> BYTEVECTOR,
      "PROOFC"    -> BYTEVECTOR,
      "ASSETID"   -> optionByteVector
    )
  )

  private def err[R](msg: String) : TrampolinedExecResult[R] = EitherT.leftT[Coeval,R](msg)

  private def proofBinding(tx: Transaction, x: Int): LazyVal =
    LazyVal(BYTEVECTOR)(tx match {
      case pt: ProvenTransaction =>
        val proof: ByteVector =
          if (x >= pt.proofs.proofs.size)
            ByteVector.empty
          else ByteVector(pt.proofs.proofs(x).arr)
        EitherT.pure(proof)
      case _ => err("Transaction doesn't contain proofs")
    })

  private def transactionObject(transaction: Coeval[Transaction]): Coeval[Obj] = transaction map { tx =>
    Obj(
      Map(
        "TYPE" -> LazyVal(INT)(EitherT.pure(tx.transactionType.id)),
        "ID"   -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(tx.id().arr))),
        "BODYBYTES" -> LazyVal(BYTEVECTOR)(tx match {
          case pt: ProvenTransaction => EitherT.pure(ByteVector(pt.bodyBytes))
          case _                     => err("Transaction doesn't contain body bytes")
        }),
        "SENDERPK" -> LazyVal(BYTEVECTOR)(tx match {
          case pt: Authorized => EitherT.pure(ByteVector(pt.sender.publicKey))
          case _              => err("Transaction doesn't contain sender public key")
        }),
        "ASSETID" -> LazyVal(optionByteVector)(tx match {
          case tt: TransferTransaction => EitherT.pure(tt.assetId.map(x => ByteVector(x.arr)).asInstanceOf[optionByteVector.Underlying]
          case _                       => err("Transaction doesn't contain asset id")
        }),
        "PROOFA" -> proofBinding(tx, 0),
        "PROOFB" -> proofBinding(tx, 1),
        "PROOFC" -> proofBinding(tx, 2)
      ))
  }
}

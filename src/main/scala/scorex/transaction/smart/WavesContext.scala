package scorex.transaction.smart

import cats.data.EitherT
import com.wavesplatform.crypto
import com.wavesplatform.lang.Context._
import com.wavesplatform.lang.Evaluator.TrampolinedExecResult
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang._
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{Authorized, ProvenTransaction, Transaction}

object WavesContext {

  val optionByteVector = OPTION(BYTEVECTOR)

  val transactionType = PredefType(
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

  val sigVerify: PredefFunction = PredefFunction("SIGVERIFY", BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
    case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil =>
      Right(crypto.verify(s.toArray, m.toArray, p.toArray))
    case _ => ???
  }

  private def err[R](msg: String): TrampolinedExecResult[R] = EitherT.leftT[Coeval, R](msg)

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

  private def transactionObject(tx: Transaction): Obj =
    Obj(
      Map(
        "TYPE" -> LazyVal(INT)(EitherT.pure(tx.transactionType.id)),
        "ID"   -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(tx.id().arr))),
        "BODYBYTES" -> LazyVal(BYTEVECTOR)(tx match {
          case pt: ProvenTransaction => EitherT.pure(ByteVector(pt.bodyBytes()))
          case _                     => err("Transaction doesn't contain body bytes")
        }),
        "SENDERPK" -> LazyVal(BYTEVECTOR)(tx match {
          case pt: Authorized => EitherT.pure(ByteVector(pt.sender.publicKey))
          case _              => err("Transaction doesn't contain sender public key")
        }),
        "ASSETID" -> LazyVal(optionByteVector)(tx match {
          case tt: TransferTransaction => EitherT.pure(tt.assetId.map(x => ByteVector(x.arr)).asInstanceOf[optionByteVector.Underlying])
          case _                       => err("Transaction doesn't contain asset id")
        }),
        "PROOFA" -> proofBinding(tx, 0),
        "PROOFB" -> proofBinding(tx, 1),
        "PROOFC" -> proofBinding(tx, 2)
      ))

  private def getTxById(state: SnapshotStateReader) = {
    val returnType = OPTION(TYPEREF(transactionType.name))
    PredefFunction("GETTRANSACTIONBYID", returnType, List(("id", BYTEVECTOR))) {
      case (id: ByteVector) :: Nil =>
        val maybeTx: Option[Transaction] = state.transactionInfo(ByteStr(id.toArray)).map(_._2)
        val maybeDomainTx                = maybeTx.map(transactionObject)
        Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
      case _ => ???
    }
  }

  def build(tx: Coeval[Transaction], height: Coeval[Int], state: SnapshotStateReader): Context = {
    val txById = getTxById(state)

    Context(
      Map(transactionType.name -> transactionType),
      Map(("H", LazyVal(INT)(EitherT.right(height))), ("TX", LazyVal(TYPEREF(transactionType.name))(EitherT.right(tx.map(transactionObject))))),
      Map(sigVerify.name -> sigVerify, txById.name -> txById)
    )
  }

}

package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.traits.{Crypto, Emulator, Transaction}
import monix.eval.Coeval
import scodec.bits.ByteVector

abstract class WavesContextImpl { this: Crypto with Emulator =>

  import WavesContextImpl._

  val keccack256F: PredefFunction = hashFunction("keccack256")(this.keccack256)
  val blake2b256F: PredefFunction = hashFunction("blake2b256")(this.blake2b256)
  val sha256F: PredefFunction     = hashFunction("sha256")(this.sha256)

  val sigVerifyF: PredefFunction = PredefFunction("sigVerify", BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
    case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil =>
      Right(this.curve25519verify(m.toArray, s.toArray, p.toArray))
    case _ => ???
  }

  private def proofBinding(tx: Transaction, x: Int): LazyVal =
    LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.proofs map { pfs =>
      if (x >= pfs.size)
        ByteVector.empty
      else pfs(x)
    }))

  private def transactionObject(tx: Transaction): Obj =
    Obj(
      Map(
        "type"      -> LazyVal(LONG)(EitherT.pure(tx.transactionType)),
        "id"        -> LazyVal(BYTEVECTOR)(EitherT.pure(tx.id)),
        "bodyBytes" -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.bodyBytes)),
        "senderPk"  -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.senderPk)),
        "assetId"   -> LazyVal(optionByteVector)(EitherT.fromEither(tx.assetId.map(_.asInstanceOf[optionByteVector.Underlying]))),
        "proof0"    -> proofBinding(tx, 0),
        "proof1"    -> proofBinding(tx, 1),
        "proof2"    -> proofBinding(tx, 2)
      ))

  private val txByIdF = {
    val returnType = OPTION(TYPEREF(transactionType.name))
    PredefFunction("getTransactionById", returnType, List(("id", BYTEVECTOR))) {
      case (id: ByteVector) :: Nil =>
        val maybeDomainTx = transactionById(id.toArray).map(transactionObject)
        Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
      case _ => ???
    }
  }

  def build(): Context = {
    val txCoeval: Coeval[Either[String, Obj]]               = Coeval.evalOnce(Right(transactionObject(transaction)))
    val heightCoeval: Coeval[Either[String, Long]]          = Coeval.evalOnce(Right(height))
    Context(
      Map(transactionType.name -> transactionType),
      Map(
        ("None", none),
        ("H", LazyVal(LONG)(EitherT(heightCoeval))),
        ("tx", LazyVal(TYPEREF(transactionType.name))(EitherT(txCoeval)))
      ),
      Map(
        sigVerifyF.name -> sigVerifyF,
        txByIdF.name    -> txByIdF,
        extract.name    -> extract,
        isDefined.name  -> isDefined,
        some.name       -> some,
        //hashing
        keccack256F.name -> keccack256F,
        blake2b256F.name -> blake2b256F,
        sha256F.name     -> sha256F
      )
    )
  }
}
object WavesContextImpl {

  private val noneCoeval: Coeval[Either[String, Option[Nothing]]] = Coeval.evalOnce(Right(None))
  val none: LazyVal = LazyVal(OPTION(NOTHING))(EitherT(noneCoeval))
  private val optionByteVector = OPTION(BYTEVECTOR)
  private val optionT          = OPTIONTYPEPARAM(TYPEPARAM('T'))

  private def hashFunction(name: String)(h: Array[Byte] => Array[Byte]) = PredefFunction(name, BYTEVECTOR, List(("bytes", BYTEVECTOR))) {
    case (m: ByteVector) :: Nil => Right(ByteVector(h(m.toArray)))
    case _                      => ???
  }

  val transactionType = PredefType(
    "Transaction",
    List(
      "type"      -> LONG,
      "id"        -> BYTEVECTOR,
      "bodyBytes" -> BYTEVECTOR,
      "senderPk"  -> BYTEVECTOR,
      "proof0"    -> BYTEVECTOR,
      "proof1"    -> BYTEVECTOR,
      "proof2"    -> BYTEVECTOR,
      "assetId"   -> optionByteVector
    )
  )
  val extract: PredefFunction = PredefFunction("extract", TYPEPARAM('T'), List(("opt", optionT))) {
    case Some(v) :: Nil => Right(v)
    case None :: Nil    => Left("Extract from empty option")
    case _              => ???
  }

  val some: PredefFunction = PredefFunction("Some", optionT, List(("obj", TYPEPARAM('T')))) {
    case v :: Nil => Right(Some(v))
    case _        => ???
  }

  val isDefined: PredefFunction = PredefFunction("isDefined", BOOLEAN, List(("opt", optionT))) {
    case Some(_) :: Nil => Right(true)
    case None :: Nil    => Right(false)
    case _              => ???
  }

}

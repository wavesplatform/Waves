package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.traits._
import monix.eval.Coeval
import scodec.bits.ByteVector

abstract class BaseWavesContext extends Environment {

  import BaseWavesContext._

  private val Global           = com.wavesplatform.lang.hacks.Global // Hack for IDEA
  private val environmentFuncs = new EnvironmentFunctions(this)

  val keccak256F: PredefFunction  = hashFunction("keccak256")(Global.keccak256)
  val blake2b256F: PredefFunction = hashFunction("blake2b256")(Global.blake2b256)
  val sha256F: PredefFunction     = hashFunction("sha256")(Global.sha256)

  val sigVerifyF: PredefFunction = PredefFunction("sigVerify", BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
    case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil => Right(Global.curve25519verify(m.toArray, s.toArray, p.toArray))
    case _                                                            => ???
  }

  private def getDataF(name: String, dataType: DataType) =
    PredefFunction(name, OPTION(dataType.innerType), List(("address", addressType.typeRef), ("key", STRING))) {
      case (addr: Obj) :: (k: String) :: Nil => environmentFuncs.getData(addr, k, dataType)
      case _                                 => ???
    }

  val getLongF: PredefFunction      = getDataF("getLong", DataType.Long)
  val getBooleanF: PredefFunction   = getDataF("getBoolean", DataType.Boolean)
  val getByteArrayF: PredefFunction = getDataF("getByteArray", DataType.ByteArray)

  private def proofBinding(tx: Transaction, x: Int): LazyVal =
    LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.proofs map { pfs =>
      if (x >= pfs.size)
        ByteVector.empty
      else pfs(x)
    }))

  private def transactionObject(tx: Transaction): Obj =
    Obj(
      Map(
        "type"       -> LazyVal(LONG)(EitherT.pure(tx.transactionType)),
        "id"         -> LazyVal(BYTEVECTOR)(EitherT.pure(tx.id)),
        "fee"        -> LazyVal(LONG)(EitherT.pure(tx.fee)),
        "amount"     -> LazyVal(LONG)(EitherT.fromEither(tx.amount)),
        "feeAssetId" -> LazyVal(optionByteVector)(EitherT.pure(tx.feeAssetId.map(_.asInstanceOf[optionByteVector.innerType.Underlying]))),
        "timestamp"  -> LazyVal(LONG)(EitherT.pure(tx.timestamp)),
        "bodyBytes"  -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.bodyBytes)),
        "senderPk"   -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.senderPk)),
        "assetId"    -> LazyVal(optionByteVector)(EitherT.fromEither(tx.assetId.map(_.asInstanceOf[optionByteVector.Underlying]))),
        "recipient" -> LazyVal(addressOrAliasType.typeRef)(EitherT.fromEither(tx.recipient.map(bv =>
          Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(bv))))))),
        "proof0" -> proofBinding(tx, 0),
        "proof1" -> proofBinding(tx, 1),
        "proof2" -> proofBinding(tx, 2),
        "proof3" -> proofBinding(tx, 3),
        "proof4" -> proofBinding(tx, 4),
        "proof5" -> proofBinding(tx, 5),
        "proof6" -> proofBinding(tx, 6),
        "proof7" -> proofBinding(tx, 7)
      ))

  private val txByIdF = {
    val returnType = OPTION(transactionType.typeRef)
    PredefFunction("getTransactionById", returnType, List(("id", BYTEVECTOR))) {
      case (id: ByteVector) :: Nil =>
        val maybeDomainTx = transactionById(id.toArray).map(transactionObject)
        Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
      case _ => ???
    }
  }

  def build(): Context = {
    val txCoeval: Coeval[Either[String, Obj]]      = Coeval.evalOnce(Right(transactionObject(transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(height))
    Context(
      Map(transactionType.name -> transactionType, addressType.name -> addressType, addressOrAliasType.name -> addressOrAliasType),
      Map(
        ("None", none),
        ("height", LazyVal(LONG)(EitherT(heightCoeval))),
        ("tx", LazyVal(TYPEREF(transactionType.name))(EitherT(txCoeval)))
      ),
      Seq(
        sigVerifyF,
        extract,
        isDefined,
        some,
        size,
        //hashing
        keccak256F,
        blake2b256F,
        sha256F,
        //utils
        toBase58StringF,
        //dsl
        addressFromPublicKeyF,
        addressFromStringF,
        //state
        txByIdF,
        getLongF,
        getBooleanF,
        getByteArrayF,
        addressFromRecipientF
      ).map(x => x.name -> x)(collection.breakOut)
    )
  }

  val toBase58StringF: PredefFunction = PredefFunction("toBase58String", STRING, List(("bytes", BYTEVECTOR))) {
    case (bytes: ByteVector) :: Nil =>
      import scorex.crypto.encode.Base58
      Right(Base58.encode(bytes.toArray))
    case _ => ???
  }

  val addressFromPublicKeyF: PredefFunction = PredefFunction("addressFromPublicKey", addressType.typeRef, List(("publicKey", BYTEVECTOR))) {
    case (pk: ByteVector) :: Nil => Right(Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(environmentFuncs.addressFromPublicKey(pk))))))
    case _                       => ???
  }

  val addressFromStringF: PredefFunction = PredefFunction("addressFromString", optionAddress, List(("string", STRING))) {
    case (addressString: String) :: Nil =>
      environmentFuncs
        .addressFromString(addressString)
        .map(_.map { address =>
          Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(address))))
        })
    case _ => ???
  }

  val addressFromRecipientF: PredefFunction =
    PredefFunction("addressFromRecipient", addressType.typeRef, List(("AddressOrAlias", TYPEREF(addressOrAliasType.name)))) {
      case Obj(fields) :: Nil =>
        environmentFuncs.addressFromRecipient(fields).map { resolved =>
          Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(resolved)))))
        }

      case _ => ???
    }
}

object BaseWavesContext {

  val addressType        = PredefType("Address", List("bytes"        -> BYTEVECTOR))
  val addressOrAliasType = PredefType("AddressOrAlias", List("bytes" -> BYTEVECTOR))

  private val noneCoeval: Coeval[Either[String, Option[Nothing]]] = Coeval.evalOnce(Right(None))

  val none: LazyVal = LazyVal(OPTION(NOTHING))(EitherT(noneCoeval).subflatMap(Right(_: Option[Nothing]))) // IDEA HACK

  private val optionByteVector: OPTION = OPTION(BYTEVECTOR)
  private val optionT                  = OPTIONTYPEPARAM(TYPEPARAM('T'))
  private val optionAddress            = OPTION(addressType.typeRef)

  private def hashFunction(name: String)(h: Array[Byte] => Array[Byte]) = PredefFunction(name, BYTEVECTOR, List(("bytes", BYTEVECTOR))) {
    case (m: ByteVector) :: Nil => Right(ByteVector(h(m.toArray)))
    case _                      => ???
  }

  val blockType = PredefType("Block",
                             List(
                               "height"    -> LONG,
                               "timestamp" -> LONG
                             ))

  val transactionType = PredefType(
    "Transaction",
    List(
      "type"       -> LONG,
      "id"         -> BYTEVECTOR,
      "fee"        -> LONG,
      "feeAssetId" -> optionByteVector,
      "timestamp"  -> LONG,
      "amount"     -> LONG,
      "bodyBytes"  -> BYTEVECTOR,
      "senderPk"   -> BYTEVECTOR,
      "proof0"     -> BYTEVECTOR,
      "proof1"     -> BYTEVECTOR,
      "proof2"     -> BYTEVECTOR,
      "proof3"     -> BYTEVECTOR,
      "proof4"     -> BYTEVECTOR,
      "proof5"     -> BYTEVECTOR,
      "proof6"     -> BYTEVECTOR,
      "proof7"     -> BYTEVECTOR,
      "assetId"    -> optionByteVector,
      "recipient"  -> addressOrAliasType.typeRef
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

  val size: PredefFunction = PredefFunction("size", LONG, List(("byteVector", BYTEVECTOR))) {
    case (bv: ByteVector) :: Nil => Right(bv.size)
    case _                       => ???
  }

}

package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.traits._
import monix.eval.Coeval
import scodec.bits.ByteVector

abstract class BaseWavesContext extends Environment {

  import BaseWavesContext._

  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA

  val keccak256F: PredefFunction  = hashFunction("keccak256")(Global.keccak256)
  val blake2b256F: PredefFunction = hashFunction("blake2b256")(Global.blake2b256)
  val sha256F: PredefFunction     = hashFunction("sha256")(Global.sha256)

  val sigVerifyF: PredefFunction = PredefFunction("sigVerify", BOOLEAN, List(("message", BYTEVECTOR), ("sig", BYTEVECTOR), ("pub", BYTEVECTOR))) {
    case (m: ByteVector) :: (s: ByteVector) :: (p: ByteVector) :: Nil =>
      Right(Global.curve25519verify(m.toArray, s.toArray, p.toArray))
    case _ => ???
  }

  private def getdataF(name: String, dataType: DataType) =
    PredefFunction(name, OPTION(dataType.innerType), List(("address", addressType.typeRef), ("key", STRING))) {
      case (addr: Obj) :: (k: String) :: Nil =>
        val addressBytes  = addr.fields("bytes").value.value.apply().right.get.asInstanceOf[ByteVector].toArray
        val retrievedData = data(addressBytes, k, dataType)
        Right(retrievedData)
      case _ => ???
    }
  val getLongF: PredefFunction      = getdataF("getLong", DataType.Long)
  val getBooleanF: PredefFunction   = getdataF("getBoolean", DataType.Boolean)
  val getByteArrayF: PredefFunction = getdataF("getByteArray", DataType.ByteArray)

  private def proofBinding(tx: Transaction, x: Int): LazyVal =
    LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.proofs map { pfs =>
      if (x >= pfs.size)
        ByteVector.empty
      else pfs(x)
    }))

  private def transactionObject(tx: Transaction): Obj = {

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
  }

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
      Map(
        sigVerifyF.header -> sigVerifyF,
        extract.header    -> extract,
        isDefined.header  -> isDefined,
        some.header       -> some,
        size.header       -> size,
        //hashing
        keccak256F.header  -> keccak256F,
        blake2b256F.header -> blake2b256F,
        sha256F.header     -> sha256F,
        //utils
        toBase58StringF.header -> toBase58StringF,
        //dsl
        addressFromPublicKeyF.header -> addressFromPublicKeyF,
        addressFromStringF.header    -> addressFromStringF,
        //state
        txByIdF.header               -> txByIdF,
        getLongF.header              -> getLongF,
        getBooleanF.header           -> getBooleanF,
        getByteArrayF.header         -> getByteArrayF,
        addressFromRecipientF.header -> addressFromRecipientF
      )
    )
  }

  private val ChecksumLength             = 4
  private val HashLength                 = 20
  private val AddressVersion             = 1: Byte
  private val AddressLength              = 1 + 1 + ChecksumLength + HashLength
  private def secureHash(a: Array[Byte]) = Global.keccak256(Global.blake2b256(a))

  val toBase58StringF: PredefFunction = PredefFunction("toBase58String", STRING, List(("bytes", BYTEVECTOR))) {
    case (bytes: ByteVector) :: Nil =>
      import scorex.crypto.encode.Base58
      Right(Base58.encode(bytes.toArray))
    case _ => ???
  }

  val addressFromPublicKeyF: PredefFunction = PredefFunction("addressFromPublicKey", addressType.typeRef, List(("publicKey", BYTEVECTOR))) {
    case (pk: ByteVector) :: Nil =>
      val publicKeyHash   = secureHash(pk.toArray).take(HashLength)
      val withoutChecksum = AddressVersion +: networkByte +: publicKeyHash
      val bytes           = withoutChecksum ++ secureHash(withoutChecksum).take(ChecksumLength)
      Right(Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(bytes))))))
    case _ => ???
  }

  val addressFromStringF: PredefFunction = PredefFunction("addressFromString", optionAddress, List(("string", STRING))) {
    case (addressString: String) :: Nil =>
      val Prefix: String = "address:"
      val base58String   = if (addressString.startsWith(Prefix)) addressString.drop(Prefix.length) else addressString
      Global.base58Decode(base58String) match {
        case Right(addressBytes) =>
          val version = addressBytes.head
          val network = addressBytes.tail.head
          lazy val checksumCorrect = {
            val checkSum          = addressBytes.takeRight(ChecksumLength)
            val checkSumGenerated = secureHash(addressBytes.dropRight(ChecksumLength)).take(ChecksumLength)
            checkSum sameElements checkSumGenerated
          }
          if (version == AddressVersion && network == networkByte && addressBytes.length == AddressLength && checksumCorrect)
            Right(Some(Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(addressBytes)))))))
          else Right(None)
        case Left(e) => Left(e)
      }

    case _ => ???
  }

  val addressFromRecipientF: PredefFunction =
    PredefFunction("addressFromRecipient", addressType.typeRef, List(("AddressOrAlias", TYPEREF(addressOrAliasType.name)))) {
      case Obj(fields) :: Nil =>
        val bytes = fields("bytes").value.map(_.asInstanceOf[ByteVector]).value()

        bytes
          .flatMap(bv => resolveAddress(bv.toArray))
          .map(resolved => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(resolved))))))

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

  val operators: Seq[PredefFunction] = {
    def createOp(op: BINARY_OP_KIND, t: TYPE, r: TYPE)(body: (t.Underlying, t.Underlying) => r.Underlying) = {
      PredefFunction(op.symbol, t, List("a" -> t, "b" -> t)) {
        case (a: t.Underlying) :: (b: t.Underlying) :: Nil =>
          Right(body(a, b))
        case _ => ???
      }
    }
    Seq(
      createOp(SUM_OP, LONG, LONG)((a, b) => a + b),
      createOp(SUM_OP, BYTEVECTOR, BYTEVECTOR)((a, b) => ByteVector(a.toArray ++ b.toArray)),
      createOp(EQ_OP, LONG, BOOLEAN)((a, b) => a == b),
      createOp(EQ_OP, BYTEVECTOR, BOOLEAN)((a, b) => a == b),
      createOp(EQ_OP, BOOLEAN, BOOLEAN)((a, b) => a == b),
      createOp(GE_OP, LONG, BOOLEAN)((a, b) => a >= b),
      createOp(GT_OP, LONG, BOOLEAN)((a, b) => a > b)
    )
  }
  val baseContext = Context.build(types = Seq.empty, letDefs = Map(("None", none)), functions = Seq(extract, isDefined, some, size) ++ operators)

  val cryptoContext = ???
}

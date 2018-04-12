package com.wavesplatform.lang.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.BaseGlobal
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.traits.{DataType, Environment, Transaction}
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  private val addressType        = PredefType("Address", List("bytes"        -> BYTEVECTOR))
  private val addressOrAliasType = PredefType("AddressOrAlias", List("bytes" -> BYTEVECTOR))

  private val optionByteVector: OPTION = OPTION(BYTEVECTOR)
  private val optionAddress            = OPTION(addressType.typeRef)

  private val transactionType = PredefType(
    "Transaction",
    List(
      "type"             -> LONG,
      "id"               -> BYTEVECTOR,
      "fee"              -> LONG,
      "feeAssetId"       -> optionByteVector,
      "timestamp"        -> LONG,
      "amount"           -> LONG,
      "bodyBytes"        -> BYTEVECTOR,
      "senderPk"         -> BYTEVECTOR,
      "aliasText"        -> STRING,
      "assetName"        -> BYTEVECTOR,
      "assetDescription" -> BYTEVECTOR,
      "attachment"       -> BYTEVECTOR,
      "decimals"         -> LONG,
      "chainId"          -> LONG,
      "version"          -> LONG,
      "reissuable"       -> BOOLEAN,
      "proof0"           -> BYTEVECTOR,
      "proof1"           -> BYTEVECTOR,
      "proof2"           -> BYTEVECTOR,
      "proof3"           -> BYTEVECTOR,
      "proof4"           -> BYTEVECTOR,
      "proof5"           -> BYTEVECTOR,
      "proof6"           -> BYTEVECTOR,
      "proof7"           -> BYTEVECTOR,
      "assetId"          -> optionByteVector,
      "recipient"        -> addressOrAliasType.typeRef
    )
  )
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
        "attachment"       -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.attachment)),
        "assetName"        -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.assetName)),
        "assetDescription" -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.assetDescription)),
        "reissuable"       -> LazyVal(BOOLEAN)(EitherT.fromEither(tx.reissuable)),
        "aliasText"        -> LazyVal(STRING)(EitherT.fromEither(tx.aliasText)),
        "decimals"         -> LazyVal(LONG)(EitherT.fromEither(tx.decimals.map(_.toLong))),
        "chainId"          -> LazyVal(LONG)(EitherT.fromEither(tx.chainId.map(_.toLong))),
        "version"          -> LazyVal(LONG)(EitherT.fromEither(tx.version.map(_.toLong))),
        "proof0"           -> proofBinding(tx, 0),
        "proof1"           -> proofBinding(tx, 1),
        "proof2"           -> proofBinding(tx, 2),
        "proof3"           -> proofBinding(tx, 3),
        "proof4"           -> proofBinding(tx, 4),
        "proof5"           -> proofBinding(tx, 5),
        "proof6"           -> proofBinding(tx, 6),
        "proof7"           -> proofBinding(tx, 7)
      ))

  def build(env: Environment, global: BaseGlobal): Context = {
    def getdataF(name: String, dataType: DataType) =
      PredefFunction(name, OPTION(dataType.innerType), List(("address", addressType.typeRef), ("key", STRING))) {
        case (addr: Obj) :: (k: String) :: Nil =>
          val addressBytes  = addr.fields("bytes").value.value.apply().right.get.asInstanceOf[ByteVector].toArray
          val retrievedData = env.data(addressBytes, k, dataType)
          Right(retrievedData)
        case _ => ???
      }

    val getLongF: PredefFunction      = getdataF("getLong", DataType.Long)
    val getBooleanF: PredefFunction   = getdataF("getBoolean", DataType.Boolean)
    val getByteArrayF: PredefFunction = getdataF("getByteArray", DataType.ByteArray)

    val ChecksumLength = 4
    val HashLength     = 20
    val AddressVersion = 1: Byte
    val AddressLength  = 1 + 1 + ChecksumLength + HashLength

    def secureHash(a: Array[Byte]) = global.keccak256(global.blake2b256(a))

    val addressFromPublicKeyF: PredefFunction = PredefFunction("addressFromPublicKey", addressType.typeRef, List(("publicKey", BYTEVECTOR))) {
      case (pk: ByteVector) :: Nil =>
        val publicKeyHash   = secureHash(pk.toArray).take(HashLength)
        val withoutChecksum = AddressVersion +: env.networkByte +: publicKeyHash
        val bytes           = withoutChecksum ++ secureHash(withoutChecksum).take(ChecksumLength)
        Right(Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(bytes))))))
      case _ => ???
    }

    val addressFromStringF: PredefFunction = PredefFunction("addressFromString", optionAddress, List(("string", STRING))) {
      case (addressString: String) :: Nil =>
        val Prefix: String = "address:"
        val base58String   = if (addressString.startsWith(Prefix)) addressString.drop(Prefix.length) else addressString
        global.base58Decode(base58String) match {
          case Right(addressBytes) =>
            val version = addressBytes.head
            val network = addressBytes.tail.head
            lazy val checksumCorrect = {
              val checkSum          = addressBytes.takeRight(ChecksumLength)
              val checkSumGenerated = secureHash(addressBytes.dropRight(ChecksumLength)).take(ChecksumLength)
              checkSum sameElements checkSumGenerated
            }
            if (version == AddressVersion && network == env.networkByte && addressBytes.length == AddressLength && checksumCorrect)
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
            .flatMap(bv => env.resolveAddress(bv.toArray))
            .map(resolved => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(resolved))))))

        case _ => ???
      }

    val txCoeval: Coeval[Either[String, Obj]]      = Coeval.evalOnce(Right(transactionObject(env.transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val txByIdF = {
      val returnType = OPTION(transactionType.typeRef)
      PredefFunction("getTransactionById", returnType, List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
        case _ => ???
      }
    }

    val accountBalanceF: PredefFunction =
      PredefFunction("accountBalance", LONG, List(("addressOrAlias", TYPEREF(addressOrAliasType.name)))) {
        case Obj(fields) :: Nil =>
          fields("bytes").value
            .map(_.asInstanceOf[ByteVector].toArray)
            .map(env.accountBalanceOf(_, None))
            .value()

        case _ => ???
      }

    val accountAssetBalanceF: PredefFunction =
      PredefFunction("accountAssetBalance", LONG, List(("addressOrAlias", TYPEREF(addressOrAliasType.name)), ("assetId", BYTEVECTOR))) {
        case Obj(fields) :: (assetId: ByteVector) :: Nil =>
          fields("bytes").value
            .map(_.asInstanceOf[ByteVector].toArray)
            .map(env.accountBalanceOf(_, Some(assetId.toArray)))
            .value()

        case _ => ???
      }

    val txHeightByIdF =
      PredefFunction("transactionHeightById", OPTION(LONG), List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil => Right(env.transactionHeightById(id.toArray))
        case _                       => ???
      }

    Context.build(
      Seq(addressType, addressOrAliasType, transactionType),
      Map(("height", LazyVal(LONG)(EitherT(heightCoeval))), ("tx", LazyVal(TYPEREF(transactionType.name))(EitherT(txCoeval)))),
      Seq(
        txByIdF,
        txHeightByIdF,
        getLongF,
        getBooleanF,
        getByteArrayF,
        addressFromPublicKeyF,
        addressFromStringF,
        addressFromRecipientF,
        accountBalanceF,
        accountAssetBalanceF
      )
    )
  }
}

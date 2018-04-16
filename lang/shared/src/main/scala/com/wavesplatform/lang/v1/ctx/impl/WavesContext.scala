package com.wavesplatform.lang.v1.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.EnvironmentFunctions
import com.wavesplatform.lang.v1.Terms._
import com.wavesplatform.lang.v1.ctx._
import com.wavesplatform.lang.v1.traits.{DataType, Environment, Transaction}
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

  // @TODO: CALCULATE COSTS
  def build(env: Environment): Context = {
    val environmentFunctions = new EnvironmentFunctions(env)

    def getdataF(name: String, dataType: DataType) =
      PredefFunction(name, 90000, OPTION(dataType.innerType), List(("address", addressType.typeRef), ("key", STRING))) {
        case (addr: Obj) :: (k: String) :: Nil => environmentFunctions.getData(addr, k, dataType)
        case _                                 => ???
      }

    val getLongF: PredefFunction      = getdataF("getLong", DataType.Long)
    val getBooleanF: PredefFunction   = getdataF("getBoolean", DataType.Boolean)
    val getByteArrayF: PredefFunction = getdataF("getByteArray", DataType.ByteArray)

    val addressFromPublicKeyF: PredefFunction = PredefFunction("addressFromPublicKey", 7500, addressType.typeRef, List(("publicKey", BYTEVECTOR))) {
      case (pk: ByteVector) :: Nil =>
        val r = environmentFunctions.addressFromPublicKey(pk)
        Right(Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(r)))))
      case _ => ???
    }

    val addressFromStringF: PredefFunction = PredefFunction("addressFromString", 6000, optionAddress, List(("string", STRING))) {
      case (addressString: String) :: Nil =>
        val r = environmentFunctions.addressFromString(addressString)
        r.map(_.map(x => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(x))))))
      case _ => ???
    }

    val addressFromRecipientF: PredefFunction =
      PredefFunction("addressFromRecipient", 14500, addressType.typeRef, List(("AddressOrAlias", TYPEREF(addressOrAliasType.name)))) {
        case Obj(fields) :: Nil =>
          val r = environmentFunctions.addressFromRecipient(fields)
          r.map(resolved => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(resolved))))))
        case _ => ???
      }

    val txCoeval: Coeval[Either[String, Obj]]      = Coeval.evalOnce(Right(transactionObject(env.transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val txByIdF = {
      val returnType = OPTION(transactionType.typeRef)
      PredefFunction("getTransactionById", 37000, returnType, List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
        case _ => ???
      }
    }

    val accountBalanceF: PredefFunction =
      PredefFunction("accountBalance", 200000, LONG, List(("addressOrAlias", TYPEREF(addressOrAliasType.name)))) {
        case Obj(fields) :: Nil =>
          fields("bytes").value
            .map(_.asInstanceOf[ByteVector].toArray)
            .map(env.accountBalanceOf(_, None))
            .value()

        case _ => ???
      }

    val accountAssetBalanceF: PredefFunction =
      PredefFunction("accountAssetBalance", 200000, LONG, List(("addressOrAlias", TYPEREF(addressOrAliasType.name)), ("assetId", BYTEVECTOR))) {
        case Obj(fields) :: (assetId: ByteVector) :: Nil =>
          fields("bytes").value
            .map(_.asInstanceOf[ByteVector].toArray)
            .map(env.accountBalanceOf(_, Some(assetId.toArray)))
            .value()

        case _ => ???
      }

    val txHeightByIdF =
      PredefFunction("transactionHeightById", 25000, OPTION(LONG), List(("id", BYTEVECTOR))) {
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

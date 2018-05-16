package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
//import com.wavesplatform.lang.v1.EnvironmentFunctions
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.traits.{DataType, Environment, Transaction, Transfer, AddressOrAlias}
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  private val addressType        = PredefType("Address", List("bytes"        -> BYTEVECTOR))
  private val addressOrAliasType = PredefType("AddressOrAlias", List("bytes" -> BYTEVECTOR))

  private def addressOrAliasObject(address: AddressOrAlias): Obj =
    Obj(
      Map(
        "bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(address.byteVector))
      )
    )

  private val optionByteVector: OPTION = OPTION(BYTEVECTOR)
  private val listByteVector: LIST     = LIST(BYTEVECTOR)
  private val optionAddress            = OPTION(addressType.typeRef)
  private val optionLong: OPTION       = OPTION(LONG)

  private val transferType = PredefType(
    "Transfer",
    List(
      "address" -> addressOrAliasType.typeRef,
      "amount"  -> LONG
    )
  )

  private val listTransfers = LIST(transferType.typeRef)

  private def transferObject(tf: listTransfers.innerType.Underlying /*Transfer*/ ): Obj =
    Obj(
      Map(
        "amount"  -> LazyVal(LONG)(EitherT.pure(tf.asInstanceOf[Transfer].amount)),
        "address" -> LazyVal(addressOrAliasType.typeRef)(EitherT.pure(addressOrAliasObject(tf.asInstanceOf[Transfer].address)))
      )
    )

  private val transactionType = PredefType(
    "Transaction",
    List(
      "type"                 -> LONG,
      "id"                   -> BYTEVECTOR,
      "fee"                  -> LONG,
      "feeAssetId"           -> optionByteVector,
      "timestamp"            -> LONG,
      "amount"               -> LONG,
      "bodyBytes"            -> BYTEVECTOR,
      "senderPk"             -> BYTEVECTOR,
      "aliasText"            -> STRING,
      "assetName"            -> BYTEVECTOR,
      "assetDescription"     -> BYTEVECTOR,
      "attachment"           -> BYTEVECTOR,
      "decimals"             -> LONG,
      "chainId"              -> LONG,
      "version"              -> LONG,
      "reissuable"           -> BOOLEAN,
      "proofs"               -> listByteVector,
      "transferAssetId"      -> optionByteVector,
      "assetId"              -> BYTEVECTOR,
      "recipient"            -> addressOrAliasType.typeRef,
      "minSponsoredAssetFee" -> optionLong,
      "transfers"            -> LIST(transferType.typeRef)
    )
  )

  class IndexedSeqWithDefault[T](content: IndexedSeq[T], default: T) extends IndexedSeq[T] {
    override def apply(idx: Int): T = {
       if(idx < content.length) {
          content(idx)
       } else {
          default
       }
    }
    override def length: Int = content.length
  }

  private def transactionObject(tx: Transaction): Obj =
    Obj(
      Map(
        "type"            -> LazyVal(LONG)(EitherT.pure(tx.transactionType)),
        "id"              -> LazyVal(BYTEVECTOR)(EitherT.pure(tx.id)),
        "fee"             -> LazyVal(LONG)(EitherT.pure(tx.fee)),
        "amount"          -> LazyVal(LONG)(EitherT.fromEither(tx.amount)),
        "feeAssetId"      -> LazyVal(optionByteVector)(EitherT.pure(tx.feeAssetId.map(_.asInstanceOf[optionByteVector.innerType.Underlying]))),
        "timestamp"       -> LazyVal(LONG)(EitherT.pure(tx.timestamp)),
        "bodyBytes"       -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.bodyBytes)),
        "senderPk"        -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.senderPk)),
        "transferAssetId" -> LazyVal(optionByteVector)(EitherT.fromEither(tx.transferAssetId.map(_.asInstanceOf[optionByteVector.Underlying]))),
        "assetId"         -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.assetId)),
        "recipient" -> LazyVal(addressOrAliasType.typeRef)(EitherT.fromEither(tx.recipient.map(bv =>
          Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(bv))))))),
        "attachment"           -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.attachment)),
        "assetName"            -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.assetName)),
        "assetDescription"     -> LazyVal(BYTEVECTOR)(EitherT.fromEither(tx.assetDescription)),
        "reissuable"           -> LazyVal(BOOLEAN)(EitherT.fromEither(tx.reissuable)),
        "aliasText"            -> LazyVal(STRING)(EitherT.fromEither(tx.aliasText)),
        "decimals"             -> LazyVal(LONG)(EitherT.fromEither(tx.decimals.map(_.toLong))),
        "chainId"              -> LazyVal(LONG)(EitherT.fromEither(tx.chainId.map(_.toLong))),
        "version"              -> LazyVal(LONG)(EitherT.fromEither(tx.version.map(_.toLong))),
        "minSponsoredAssetFee" -> LazyVal(optionLong)(EitherT.fromEither(tx.minSponsoredAssetFee.map(_.asInstanceOf[optionLong.Underlying]))),
        "transfers" -> LazyVal(listTransfers)(EitherT.fromEither(tx.transfers.map(tl =>
          tl.map(t => transferObject(t.asInstanceOf[listTransfers.innerType.Underlying])).asInstanceOf[listTransfers.Underlying]))),
        "proofs"           -> LazyVal(listByteVector)(EitherT.fromEither(tx.proofs.map(p =>
                                     new IndexedSeqWithDefault(p.asInstanceOf[listByteVector.Underlying], ByteVector.empty.asInstanceOf[listByteVector.innerType.Underlying]))))
      ))

  def build(env: Environment): EvaluationContext = {
    val environmentFunctions = new EnvironmentFunctions(env)

    def getdataF(name: String, dataType: DataType) =
      PredefFunction(name, 100, OPTION(dataType.innerType), List(("address", addressType.typeRef), ("key", STRING))) {
        case (addr: Obj) :: (k: String) :: Nil => environmentFunctions.getData(addr, k, dataType)
        case _                                 => ???
      }

    val getLongF: PredefFunction      = getdataF("getLong", DataType.Long)
    val getBooleanF: PredefFunction   = getdataF("getBoolean", DataType.Boolean)
    val getByteArrayF: PredefFunction = getdataF("getByteArray", DataType.ByteArray)

    val addressFromPublicKeyF: PredefFunction = PredefFunction("addressFromPublicKey", 100, addressType.typeRef, List(("publicKey", BYTEVECTOR))) {
      case (pk: ByteVector) :: Nil =>
        val r = environmentFunctions.addressFromPublicKey(pk)
        Right(Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(r)))))
      case _ => ???
    }

    val addressFromStringF: PredefFunction = PredefFunction("addressFromString", 100, optionAddress, List(("string", STRING))) {
      case (addressString: String) :: Nil =>
        val r = environmentFunctions.addressFromString(addressString)
        r.map(_.map(x => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(x))))))
      case _ => ???
    }

    val addressFromRecipientF: PredefFunction =
      PredefFunction("addressFromRecipient", 100, addressType.typeRef, List(("AddressOrAlias", TYPEREF(addressOrAliasType.name)))) {
        case Obj(fields) :: Nil =>
          val r = environmentFunctions.addressFromRecipient(fields)
          r.map(resolved => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(ByteVector(resolved))))))
        case _ => ???
      }

    val txCoeval: Coeval[Either[String, Obj]]      = Coeval.evalOnce(Right(transactionObject(env.transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val txByIdF = {
      val returnType = OPTION(transactionType.typeRef)
      PredefFunction("getTransactionById", 100, returnType, List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
        case _ => ???
      }
    }

    val accountBalanceF: PredefFunction =
      PredefFunction("accountBalance", 100, LONG, List(("addressOrAlias", TYPEREF(addressOrAliasType.name)))) {
        case Obj(fields) :: Nil =>
          fields("bytes").value
            .map(_.asInstanceOf[ByteVector].toArray)
            .map(env.accountBalanceOf(_, None))
            .value()

        case _ => ???
      }

    val accountAssetBalanceF: PredefFunction =
      PredefFunction("accountAssetBalance", 100, LONG, List(("addressOrAlias", TYPEREF(addressOrAliasType.name)), ("assetId", BYTEVECTOR))) {
        case Obj(fields) :: (assetId: ByteVector) :: Nil =>
          fields("bytes").value
            .map(_.asInstanceOf[ByteVector].toArray)
            .map(env.accountBalanceOf(_, Some(assetId.toArray)))
            .value()

        case _ => ???
      }

    val txHeightByIdF =
      PredefFunction("transactionHeightById", 100, OPTION(LONG), List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil => Right(env.transactionHeightById(id.toArray))
        case _                       => ???
      }

    EvaluationContext.build(
      types = Seq(addressType, addressOrAliasType, transactionType, transferType),
      caseTypes = Seq.empty,
      letDefs = Map(("height", LazyVal(LONG)(EitherT(heightCoeval))), ("tx", LazyVal(TYPEREF(transactionType.name))(EitherT(txCoeval)))),
      functions = Seq(
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

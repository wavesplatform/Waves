package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.traits.{DataType, Environment, Transaction}
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  private val addressType        = PredefCaseType("Address", List("bytes" -> BYTEVECTOR))
  private val aliasType          = PredefCaseType("Alias", List("alias" -> STRING))
  private val addressOrAliasType = UNION(addressType.typeRef, aliasType.typeRef)

  private val optionByteVector: OPTION = OPTION(BYTEVECTOR)
  private val listByteVector: LIST     = LIST(BYTEVECTOR)
  private val optionAddress            = OPTION(addressType.typeRef)
  private val optionLong: OPTION       = OPTION(LONG)

  private val common = List(
    "id"        -> BYTEVECTOR,
    "fee"       -> LONG,
    "timestamp" -> LONG,
    "version"   -> LONG,
  )
  private val proven = List(
    "senderPk"  -> BYTEVECTOR,
    "bodyBytes" -> BYTEVECTOR,
    "proofs"    -> listByteVector
  )

  private val transferTransactionType = PredefCaseType(
    "TransferTransaction",
    List(
      "feeAssetId"      -> optionByteVector,
      "amount"          -> LONG,
      "transferAssetId" -> BYTEVECTOR,
      "recipient"       -> addressOrAliasType,
      "attachment"      -> BYTEVECTOR
    ) ++ common ++ proven
  )

  private val leaseTransactionType = PredefCaseType(
    "LeaseTransaction",
    List(
      "amount"    -> LONG,
      "recipient" -> addressOrAliasType,
      "assetId"   -> BYTEVECTOR,
    ) ++ common ++ proven
  )

  private val issueTransactionType = PredefCaseType(
    "IssueTransaction",
    List(
      "amount"           -> LONG,
      "assetName"        -> BYTEVECTOR,
      "assetDescription" -> BYTEVECTOR,
      "reissuable"       -> BOOLEAN,
    ) ++ common ++ proven
  )

  private val reissueTransactionType = PredefCaseType(
    "ReissueTransaction",
    List(
      "amount"     -> LONG,
      "reissuable" -> BOOLEAN,
    ) ++ common ++ proven
  )

  private val transactionType =
    UNION(transferTransactionType.typeRef, leaseTransactionType.typeRef, issueTransactionType.typeRef, reissueTransactionType.typeRef)

  private def proofBinding(tx: Transaction, x: Int): Val =
    Val(BYTEVECTOR)(
      if (x >= tx.proofs.right.get.size)
        ByteVector.empty
      else tx.proofs.right.get(x)
    )

  private def commonTxPart(tx: Transaction): Map[String, Val] = Map(
    "id"        -> Val(BYTEVECTOR)(tx.id),
    "fee"       -> Val(LONG)(tx.fee),
    "timestamp" -> Val(LONG)(tx.timestamp),
    "version"   -> Val(LONG)(tx.version.right.get),
    "bodyBytes" -> Val(BYTEVECTOR)(tx.bodyBytes.right.get)
  )

  private def provenTxPart(tx: Transaction): Map[String, Val] = Map(
    "senderPk"  -> Val(BYTEVECTOR)(tx.senderPk.right.get),
    "bodyBytes" -> Val(BYTEVECTOR)(tx.bodyBytes.right.get),
    "proofs"    -> Val(listByteVector)(tx.proofs.right.get.toList.asInstanceOf[listByteVector.Underlying])
  )

  private def transactionObject(tx: Transaction): CaseObj =
    tx.transactionType match {
      case 4 =>
        CaseObj(
          transferTransactionType.typeRef,
          Map(
            "amount"     -> Val(LONG)(tx.amount.right.get),
            "feeAssetId" -> Val(optionByteVector)(tx.feeAssetId.asInstanceOf[optionByteVector.Underlying]),
//            "recipient" -> Val(addressOrAliasType)(EitherT.fromEither(tx.recipient.map(bv => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(bv))))))),
            "transferAssetId" -> Val(optionByteVector)(tx.transferAssetId.right.get.asInstanceOf[optionByteVector.Underlying]),
            "attachment"      -> Val(BYTEVECTOR)(tx.attachment.right.get)
          ) ++ commonTxPart(tx) ++ provenTxPart(tx)
        )

      case 5 =>
        CaseObj(
          issueTransactionType.typeRef,
          Map(
            "amount"           -> Val(LONG)(tx.amount.right.get),
            "assetName"        -> Val(BYTEVECTOR)(tx.assetName.right.get),
            "assetDescription" -> Val(BYTEVECTOR)(tx.assetDescription.right.get),
            "reissuable"       -> Val(BOOLEAN)(tx.reissuable.right.get),
          ) ++ commonTxPart(tx) ++ provenTxPart(tx)
        )
      case 6 =>
        CaseObj(
          reissueTransactionType.typeRef,
          Map(
            "amount"     -> Val(LONG)(tx.amount.right.get),
            "reissuable" -> Val(BOOLEAN)(tx.reissuable.right.get),
          ) ++ commonTxPart(tx) ++ provenTxPart(tx)
        )

      case 8 =>
        CaseObj(
          leaseTransactionType.typeRef,
          Map(
            //            "recipient" -> Val(addressOrAliasType)(EitherT.fromEither(tx.recipient.map(bv => Obj(Map("bytes" -> LazyVal(BYTEVECTOR)(EitherT.pure(bv))))))),
            "amount"  -> Val(LONG)(tx.amount.right.get),
            "assetId" -> Val(BYTEVECTOR)(tx.assetId.right.get),
          ) ++ commonTxPart(tx) ++ provenTxPart(tx)
        )
    }

  def build(env: Environment): EvaluationContext = {
    val environmentFunctions = new EnvironmentFunctions(env)

    def getdataF(name: String, dataType: DataType) =
      PredefFunction(name, 100, OPTION(dataType.innerType), List(("address", addressType.typeRef), ("key", STRING))) {
        case (addr: CaseObj) :: (k: String) :: Nil => environmentFunctions.getData(addr, k, dataType)
        case _                                     => ???
      }

    val getLongF: PredefFunction      = getdataF("getLong", DataType.Long)
    val getBooleanF: PredefFunction   = getdataF("getBoolean", DataType.Boolean)
    val getByteArrayF: PredefFunction = getdataF("getByteArray", DataType.ByteArray)

    val addressFromPublicKeyF: PredefFunction = PredefFunction("addressFromPublicKey", 100, addressType.typeRef, List(("publicKey", BYTEVECTOR))) {
      case (pk: ByteVector) :: Nil =>
        val r = environmentFunctions.addressFromPublicKey(pk)
        Right(CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(r))))
      case _ => ???
    }

    val addressFromStringF: PredefFunction = PredefFunction("addressFromString", 100, optionAddress, List(("string", STRING))) {
      case (addressString: String) :: Nil =>
        val r = environmentFunctions.addressFromString(addressString)
        r.map(_.map(x => CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(x)))))
      case _ => ???
    }

    val addressFromRecipientF: PredefFunction =
      PredefFunction("addressFromRecipient", 100, addressType.typeRef, List(("AddressOrAlias", addressOrAliasType))) {
        case CaseObj(_, fields) :: Nil =>
          val r = environmentFunctions.addressFromRecipient(fields)
          r.map(resolved => CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(ByteVector(resolved)))))
        case _ => ???
      }

    val txCoeval: Coeval[Either[String, CaseObj]]  = Coeval.evalOnce(Right(transactionObject(env.transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val txByIdF = {
      val returnType = OPTION(transactionType)
      PredefFunction("getTransactionById", 100, returnType, List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
        case _ => ???
      }
    }

    val accountBalanceF: PredefFunction =
      PredefFunction("accountBalance", 100, LONG, List(("addressOrAlias", addressOrAliasType))) {
        case CaseObj(_, fields) :: Nil =>
          val acc = fields("bytes").value.asInstanceOf[ByteVector].toArray
          env.accountBalanceOf(acc, None)

        case _ => ???
      }

    val accountAssetBalanceF: PredefFunction =
      PredefFunction("accountAssetBalance", 100, LONG, List(("addressOrAlias", addressOrAliasType), ("assetId", BYTEVECTOR))) {
        case CaseObj(_, fields) :: (assetId: ByteVector) :: Nil =>
          val acc = fields("bytes").value.asInstanceOf[ByteVector]
          env.accountBalanceOf(acc.toArray, Some(assetId.toArray))

        case _ => ???
      }

    val txHeightByIdF =
      PredefFunction("transactionHeightById", 100, OPTION(LONG), List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil => Right(env.transactionHeightById(id.toArray))
        case _                       => ???
      }

    EvaluationContext.build(
      caseTypes = Seq(addressType, aliasType, transferTransactionType, issueTransactionType, reissueTransactionType, leaseTransactionType),
      letDefs = Map(("height", LazyVal(LONG)(EitherT(heightCoeval))), ("tx", LazyVal(transactionType)(EitherT(txCoeval)))),
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

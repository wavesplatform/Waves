package com.wavesplatform.lang.v1.evaluator.ctx.impl

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.traits.Tx._
import com.wavesplatform.lang.v1.traits.{Tx, _}
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  private val addressType        = PredefCaseType("Address", List("bytes" -> BYTEVECTOR))
  private val aliasType          = PredefCaseType("Alias", List("alias" -> STRING))
  private val addressOrAliasType = UNION(addressType.typeRef, aliasType.typeRef)

  private val transfer = PredefCaseType("Transfer", List("recipient" -> addressOrAliasType, "amount" -> LONG))

  private val optionByteVector: OPTION = OPTION(BYTEVECTOR)
  private val optionAddress            = OPTION(addressType.typeRef)
  private val optionLong: OPTION       = OPTION(LONG)
  private val listByteVector: LIST     = LIST(BYTEVECTOR)
  private val listTransfers            = LIST(transfer.typeRef)

  private val header = List(
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

  private val genesisTransactionType = PredefCaseType(
    "GenesisTransaction",
    List("amount" -> LONG, "recipient" -> addressOrAliasType) ++ header
  )

  private val transferTransactionType = PredefCaseType(
    "TransferTransaction",
    List(
      "feeAssetId"      -> optionByteVector,
      "amount"          -> LONG,
      "transferAssetId" -> optionByteVector,
      "recipient"       -> addressOrAliasType,
      "attachment"      -> BYTEVECTOR
    ) ++ header ++ proven
  )

  private val issueTransactionType = PredefCaseType(
    "IssueTransaction",
    List(
      "amount"           -> LONG,
      "assetName"        -> BYTEVECTOR,
      "assetDescription" -> BYTEVECTOR,
      "reissuable"       -> BOOLEAN,
    ) ++ header ++ proven
  )

  private val reissueTransactionType = PredefCaseType(
    "ReissueTransaction",
    List(
      "amount"     -> LONG,
      "reissuable" -> BOOLEAN,
    ) ++ header ++ proven
  )

  private val burnTransactionType = PredefCaseType(
    "BurnTransaction",
    List(
      "amount" -> LONG,
    ) ++ header ++ proven
  )
  private val leaseTransactionType = PredefCaseType(
    "LeaseTransaction",
    List(
      "amount"    -> LONG,
      "recipient" -> addressOrAliasType,
    ) ++ header ++ proven
  )

  private val leaseCancelTransactionType = PredefCaseType(
    "LeaseCancelTransaction",
    List(
      "leaseId" -> BYTEVECTOR,
    ) ++ header ++ proven
  )

  private val createAliasTransactionType = PredefCaseType(
    "CreateAliasTransaction",
    List(
      "alias" -> STRING,
    ) ++ header ++ proven
  )

  private val paymentTransactionType = PredefCaseType(
    "PaymentTransaction",
    List(
      "amount"    -> LONG,
      "recipient" -> addressOrAliasType,
    ) ++ header ++ proven
  )

  private val sponsorFeeTransactionType = PredefCaseType(
    "SponsorFeeTransaction",
    List(
      "minFee" -> optionLong
    ) ++ header ++ proven
  )

  private val exchangeTransactionType = PredefCaseType(
    "ExchangeTransaction",
    header ++ proven
  )

  private val dataTransactionType = PredefCaseType(
    "DataTransaction",
    header ++ proven
  )

  private val massTransferTransactionType = PredefCaseType(
    "MassTransferTransaction",
    List(
      "feeAssetId"      -> optionByteVector,
      "transferAssetId" -> optionByteVector,
      "transfers"       -> listTransfers,
      "attachment"      -> BYTEVECTOR
    ) ++ header ++ proven
  )

  private val setScriptTransactionType = PredefCaseType(
    "SetScriptTransaction",
    List(
      "script" -> optionByteVector
    ) ++ header ++ proven
  )

  val obsoleteTransactionTypes = List(genesisTransactionType, paymentTransactionType)

  private val activeTransactionTypes = List(
    transferTransactionType,
    issueTransactionType,
    reissueTransactionType,
    burnTransactionType,
    leaseTransactionType,
    leaseCancelTransactionType,
    massTransferTransactionType,
    createAliasTransactionType,
    setScriptTransactionType,
    sponsorFeeTransactionType,
    exchangeTransactionType,
    dataTransactionType
  )

  val transactionTypes = obsoleteTransactionTypes ++ activeTransactionTypes

  private val transactionType = UNION(activeTransactionTypes.map(_.typeRef))

  private def headerPart(tx: Header): Map[String, Val] = Map(
    "id"        -> Val(BYTEVECTOR)(tx.id),
    "fee"       -> Val(LONG)(tx.fee),
    "timestamp" -> Val(LONG)(tx.timestamp),
    "version"   -> Val(LONG)(tx.version),
  )

  private def provenTxPart(tx: Proven): Map[String, Val] =
    Map(
      "senderPk"  -> Val(BYTEVECTOR)(tx.senderPk),
      "bodyBytes" -> Val(BYTEVECTOR)(tx.bodyBytes),
      "proofs" -> Val(listByteVector) {
        val existingProofs = tx.proofs
        val allProofs      = existingProofs ++ Seq.fill(8 - existingProofs.size)(ByteVector.empty)
        allProofs.toIndexedSeq.asInstanceOf[listByteVector.Underlying]
      }
    ) ++ headerPart(tx.h)

  private def mapRecipient(r: Recipient) =
    "recipient" -> Val(addressOrAliasType)(r match {
      case Recipient.Alias(name)    => CaseObj(aliasType.typeRef, Map("name"    -> Val(STRING)(name)))
      case Recipient.Address(bytes) => CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(bytes)))
    })

  private def transactionObject(tx: Tx): CaseObj =
    tx match {
      case Tx.Genesis(h, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> Val(LONG)(amount)) ++ headerPart(h) + mapRecipient(recipient))
      case Tx.Payment(p, amount, recipient) =>
        CaseObj(genesisTransactionType.typeRef, Map("amount" -> Val(LONG)(amount)) ++ provenTxPart(p) + mapRecipient(recipient))
      case Tx.Transfer(p, feeAssetId, transferAssetId, amount, recipient, attachment) =>
        CaseObj(
          transferTransactionType.typeRef,
          Map(
            "amount"          -> Val(LONG)(amount),
            "feeAssetId"      -> Val(optionByteVector)(feeAssetId.asInstanceOf[optionByteVector.Underlying]),
            "transferAssetId" -> Val(optionByteVector)(transferAssetId.asInstanceOf[optionByteVector.Underlying]),
            "attachment"      -> Val(BYTEVECTOR)(attachment)
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case Issue(p, amount, assetName, assetDescription, reissuable) =>
        CaseObj(
          issueTransactionType.typeRef,
          Map(
            "amount"           -> Val(LONG)(amount),
            "assetName"        -> Val(BYTEVECTOR)(assetName),
            "assetDescription" -> Val(BYTEVECTOR)(assetDescription),
            "reissuable"       -> Val(BOOLEAN)(reissuable)
          ) ++ provenTxPart(p)
        )
      case ReIssue(p, amount, reissuable) =>
        CaseObj(reissueTransactionType.typeRef,
                Map(
                  "amount"     -> Val(LONG)(amount),
                  "reissuable" -> Val(BOOLEAN)(reissuable),
                ) ++ provenTxPart(p))
      case Burn(p, amount) =>
        CaseObj(burnTransactionType.typeRef,
                Map(
                  "amount" -> Val(LONG)(amount),
                ) ++ provenTxPart(p))
      case Lease(p, amount, recipient) =>
        CaseObj(
          leaseTransactionType.typeRef,
          Map(
            "amount" -> Val(LONG)(amount),
          ) ++ provenTxPart(p) + mapRecipient(recipient)
        )
      case LeaseCancel(p, leaseId) =>
        CaseObj(
          leaseCancelTransactionType.typeRef,
          Map(
            "leaseId" -> Val(BYTEVECTOR)(leaseId),
          ) ++ provenTxPart(p)
        )
      case CreateAlias(p, alias) =>
        CaseObj(
          leaseCancelTransactionType.typeRef,
          Map(
            "alias" -> Val(STRING)(alias),
          ) ++ provenTxPart(p)
        )
      case MassTransfer(p, transferAssetId, transfers, attachment) =>
        CaseObj(
          massTransferTransactionType.typeRef,
          Map(
            "transfers" -> Val(listTransfers)(
              transfers
                .map(bv => CaseObj(transfer.typeRef, Map(mapRecipient(bv.recipient), "amount" -> Val(LONG)(bv.amount))))
                .asInstanceOf[listTransfers.Underlying]),
            "transferAssetId" -> Val(optionByteVector)(transferAssetId.asInstanceOf[optionByteVector.Underlying]),
            "attachment"      -> Val(BYTEVECTOR)(attachment)
          ) ++ provenTxPart(p)
        )
      case SetScript(p, scriptOpt) =>
        CaseObj(setScriptTransactionType.typeRef,
                Map("script" -> Val(optionByteVector)(scriptOpt.asInstanceOf[optionByteVector.Underlying])) ++ provenTxPart(p))
      case Sponsorship(p, minFee) =>
        CaseObj(sponsorFeeTransactionType.typeRef, Map("minFee" -> Val(optionLong)(minFee.asInstanceOf[optionLong.Underlying])) ++ provenTxPart(p))
      case Data(p) =>
        CaseObj(dataTransactionType.typeRef, provenTxPart(p))
      case Exchange(p) =>
        CaseObj(exchangeTransactionType.typeRef, provenTxPart(p))
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
    val getStringF: PredefFunction    = getdataF("getString", DataType.String)

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
        case (c @ CaseObj(addressType.typeRef, _)) :: Nil => Right(c)
        case c @ CaseObj(aliasType.typeRef, fields) :: Nil =>
          environmentFunctions
            .addressFromAlias(fields("name").value.asInstanceOf[String])
            .map(resolved => CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(resolved.bytes))))
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
      caseTypes = Seq(addressType, aliasType, transfer) ++ activeTransactionTypes,
      letDefs = Map(("height", LazyVal(LONG)(EitherT(heightCoeval))), ("tx", LazyVal(transactionType)(EitherT(txCoeval)))),
      functions = Seq(
        txByIdF,
        txHeightByIdF,
        getLongF,
        getBooleanF,
        getByteArrayF,
        getStringF,
        addressFromPublicKeyF,
        addressFromStringF,
        addressFromRecipientF,
        accountBalanceF,
        accountAssetBalanceF
      )
    )
  }
}

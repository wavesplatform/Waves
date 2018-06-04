package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.PredefCaseType

object Types {

  val addressType        = PredefCaseType("Address", List("bytes" -> BYTEVECTOR))
  val aliasType          = PredefCaseType("Alias", List("alias" -> STRING))
  val addressOrAliasType = UNION(addressType.typeRef, aliasType.typeRef)

  val transfer = PredefCaseType("Transfer", List("recipient" -> addressOrAliasType, "amount" -> LONG))

  val optionByteVector: OPTION = OPTION(BYTEVECTOR)
  val optionAddress            = OPTION(addressType.typeRef)
  val optionLong: OPTION       = OPTION(LONG)
  val listByteVector: LIST     = LIST(BYTEVECTOR)
  val listTransfers            = LIST(transfer.typeRef)

  val header = List(
    "id"        -> BYTEVECTOR,
    "fee"       -> LONG,
    "timestamp" -> LONG,
    "version"   -> LONG,
  )
  val proven = List(
    "senderPk"  -> BYTEVECTOR,
    "bodyBytes" -> BYTEVECTOR,
    "proofs"    -> listByteVector
  )

  val genesisTransactionType = PredefCaseType(
    "GenesisTransaction",
    List("amount" -> LONG, "recipient" -> addressOrAliasType) ++ header
  )

  val transferTransactionType = PredefCaseType(
    "TransferTransaction",
    List(
      "feeAssetId"      -> optionByteVector,
      "amount"          -> LONG,
      "transferAssetId" -> optionByteVector,
      "recipient"       -> addressOrAliasType,
      "attachment"      -> BYTEVECTOR
    ) ++ header ++ proven
  )

  val issueTransactionType = PredefCaseType(
    "IssueTransaction",
    List(
      "quantity"    -> LONG,
      "name"        -> BYTEVECTOR,
      "description" -> BYTEVECTOR,
      "reissuable"  -> BOOLEAN,
      "decimals"    -> LONG
    ) ++ header ++ proven
  )

  val reissueTransactionType = PredefCaseType(
    "ReissueTransaction",
    List(
      "quantity"   -> LONG,
      "assetId"    -> BYTEVECTOR,
      "reissuable" -> BOOLEAN,
    ) ++ header ++ proven
  )

  val burnTransactionType = PredefCaseType(
    "BurnTransaction",
    List(
      "quantity" -> LONG,
      "assetId"  -> BYTEVECTOR
    ) ++ header ++ proven
  )
  val leaseTransactionType = PredefCaseType(
    "LeaseTransaction",
    List(
      "amount"    -> LONG,
      "recipient" -> addressOrAliasType,
    ) ++ header ++ proven
  )

  val leaseCancelTransactionType = PredefCaseType(
    "LeaseCancelTransaction",
    List(
      "leaseId" -> BYTEVECTOR,
    ) ++ header ++ proven
  )

  val createAliasTransactionType = PredefCaseType(
    "CreateAliasTransaction",
    List(
      "alias" -> STRING,
    ) ++ header ++ proven
  )

  val paymentTransactionType = PredefCaseType(
    "PaymentTransaction",
    List(
      "amount"    -> LONG,
      "recipient" -> addressOrAliasType,
    ) ++ header ++ proven
  )

  val sponsorFeeTransactionType = PredefCaseType(
    "SponsorFeeTransaction",
    List(
      "assetId"              -> BYTEVECTOR,
      "minSponsoredAssetFee" -> optionLong
    ) ++ header ++ proven
  )

  val buyType     = PredefCaseType("Buy", List.empty)
  val sellType    = PredefCaseType("Buy", List.empty)
  val ordTypeType = UNION(buyType.typeRef, sellType.typeRef)

  val assetPairType = PredefCaseType("AssetPair", List("amountAsset" -> optionByteVector, "priceAsset" -> optionByteVector))
  val orderType = PredefCaseType(
    "Order",
    List(
      "senderPublicKey"  -> BYTEVECTOR,
      "matcherPublicKey" -> BYTEVECTOR,
      "assetPair"        -> assetPairType.typeRef,
      "orderType"        -> ordTypeType,
      "price"            -> LONG,
      "amount"           -> LONG,
      "timestamp"        -> LONG,
      "expiration"       -> LONG,
      "matcherFee"       -> LONG,
      "signature"        -> BYTEVECTOR
    )
  )
  val exchangeTransactionType = PredefCaseType(
    "ExchangeTransaction",
    List("buyOrder"       -> orderType.typeRef,
         "sellOrder"      -> orderType.typeRef,
         "price"          -> LONG,
         "amount"         -> LONG,
         "buyMatcherFee"  -> LONG,
         "sellMatcherFee" -> LONG)
      ++ header ++ proven
  )

  def buildDataEntryType(name: String, tpe: TYPE) = PredefCaseType(name + "DataEntry", List("key" -> STRING, "value" -> tpe))

  val strDataEntryType  = buildDataEntryType("Str", STRING)
  val boolDataEntryType = buildDataEntryType("Bool", BOOLEAN)
  val bvDataEntryType   = buildDataEntryType("ByteVector", BYTEVECTOR)
  val longDataEntryType = buildDataEntryType("Long", LONG)
  val dataEntryTypes    = List(strDataEntryType, boolDataEntryType, bvDataEntryType, longDataEntryType)

  val listOfDataEntriesType = LIST(UNION(dataEntryTypes.map(_.typeRef)))

  val dataTransactionType = PredefCaseType(
    "DataTransaction",
    List("dataEntries" -> listOfDataEntriesType) ++ header ++ proven
  )

  val massTransferTransactionType = PredefCaseType(
    "MassTransferTransaction",
    List(
      "feeAssetId"    -> optionByteVector,
      "assetId"       -> optionByteVector,
      "totalAmount"   -> LONG,
      "transfers"     -> listTransfers,
      "transferCount" -> LONG,
      "attachment"    -> BYTEVECTOR
    ) ++ header ++ proven
  )

  val setScriptTransactionType = PredefCaseType(
    "SetScriptTransaction",
    List(
      "script" -> optionByteVector
    ) ++ header ++ proven
  )

  val obsoleteTransactionTypes = List(genesisTransactionType, paymentTransactionType)

  val activeTransactionTypes = List(
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

  val outgoingTransactionType = UNION(activeTransactionTypes.map(_.typeRef))
  val anyTransactionType      = UNION(transactionTypes.map(_.typeRef))

  val caseTypes = (Seq(addressType, aliasType, transfer) ++ transactionTypes)
}

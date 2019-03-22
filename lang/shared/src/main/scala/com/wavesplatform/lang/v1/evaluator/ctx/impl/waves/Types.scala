package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.StdLibVersion
import com.wavesplatform.lang.StdLibVersion.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseType, DefinedType, UnionType}

object Types {

  val addressType        = CaseType("Address", List("bytes" -> BYTESTR))
  val aliasType          = CaseType("Alias", List("alias" -> STRING))
  val addressOrAliasType = UNION(addressType.typeRef, aliasType.typeRef)

  val transfer         = CaseType("Transfer", List("recipient" -> addressOrAliasType, "amount" -> LONG))

  val optionAddress        = UNION(addressType.typeRef, UNIT)
  val listTransfers        = LIST(transfer.typeRef)
  val paymentType          = CaseType("AttachedPayment", List("assetId" -> optionByteVector, "amount" -> LONG))

  val optionPayment = UNION(paymentType.typeRef, UNIT)

  val invocationType =
    CaseType("Invocation", List("caller" -> addressType.typeRef, "callerPublicKey" -> BYTESTR, "payment" -> optionPayment))

  private val header = List(
    "id"        -> BYTESTR,
    "fee"       -> LONG,
    "timestamp" -> LONG,
    "version"   -> LONG,
  )
  private val proven = List(
    "sender"          -> addressType.typeRef,
    "senderPublicKey" -> BYTESTR,
    "bodyBytes"       -> BYTESTR
  )

  private val proofs = "proofs" -> listByteVector

  val genesisTransactionType = CaseType(
    "GenesisTransaction",
    List("amount" -> LONG, "recipient" -> addressOrAliasType) ++ header
  )

  def buildTransferTransactionType(proofsEnabled: Boolean) = {
    CaseType(
      "TransferTransaction",
      addProofsIfNeeded(
        List(
          "feeAssetId" -> optionByteVector,
          "amount"     -> LONG,
          "assetId"    -> optionByteVector,
          "recipient"  -> addressOrAliasType,
          "attachment" -> BYTESTR
        ) ++ header ++ proven,
        proofsEnabled
      )
    )
  }

  def addProofsIfNeeded(commonFields: List[(String, FINAL)], proofsEnabled: Boolean): List[(String, FINAL)] = {
    if (proofsEnabled) commonFields :+ proofs
    else commonFields
  }

  def buildIssueTransactionType(proofsEnabled: Boolean) = CaseType(
    "IssueTransaction",
    addProofsIfNeeded(
      List(
        "quantity"    -> LONG,
        "name"        -> BYTESTR,
        "description" -> BYTESTR,
        "reissuable"  -> BOOLEAN,
        "decimals"    -> LONG,
        "script"      -> optionByteVector
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildInvokeScriptTransactionType(proofsEnabled: Boolean) = CaseType(
    "InvokeScriptTransaction",
    addProofsIfNeeded(
      List(
        "dappAddress" -> addressType.typeRef,
        "payment"     -> optionPayment,
        "feeAssetId" -> optionByteVector
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildReissueTransactionType(proofsEnabled: Boolean) = CaseType(
    "ReissueTransaction",
    addProofsIfNeeded(
      List(
        "quantity"   -> LONG,
        "assetId"    -> BYTESTR,
        "reissuable" -> BOOLEAN,
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildBurnTransactionType(proofsEnabled: Boolean) = CaseType(
    "BurnTransaction",
    addProofsIfNeeded(
      List(
        "quantity" -> LONG,
        "assetId"  -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildSetAssetScriptTransactionType(proofsEnabled: Boolean) = CaseType(
    "SetAssetScriptTransaction",
    addProofsIfNeeded(
      List(
        "script"  -> optionByteVector,
        "assetId" -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildLeaseTransactionType(proofsEnabled: Boolean) = CaseType(
    "LeaseTransaction",
    addProofsIfNeeded(
      List(
        "amount"    -> LONG,
        "recipient" -> addressOrAliasType,
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildLeaseCancelTransactionType(proofsEnabled: Boolean) = CaseType(
    "LeaseCancelTransaction",
    addProofsIfNeeded(
      List(
        "leaseId" -> BYTESTR,
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildCreateAliasTransactionType(proofsEnabled: Boolean) = CaseType(
    "CreateAliasTransaction",
    addProofsIfNeeded(
      List(
        "alias" -> STRING,
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildPaymentTransactionType(proofsEnabled: Boolean) = {
    CaseType(
      "PaymentTransaction",
      addProofsIfNeeded(
        List(
          "amount"    -> LONG,
          "recipient" -> addressOrAliasType
        ) ++ header ++ proven,
        proofsEnabled
      )
    )
  }

  def buildSponsorFeeTransactionType(proofsEnabled: Boolean) = CaseType(
    "SponsorFeeTransaction",
    addProofsIfNeeded(
      List(
        "assetId"              -> BYTESTR,
        "minSponsoredAssetFee" -> optionLong
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  val buyType  = CaseType("Buy", List.empty)
  val sellType = CaseType("Sell", List.empty)

  val ordTypeType = UNION(buyType.typeRef, sellType.typeRef)

  val assetPairType = CaseType("AssetPair", List("amountAsset" -> optionByteVector, "priceAsset" -> optionByteVector))

  def buildOrderType(proofsEnabled: Boolean) = {
    CaseType(
      "Order",
      addProofsIfNeeded(
        List(
          "id"                -> BYTESTR,
          "matcherPublicKey"  -> BYTESTR,
          "assetPair"         -> assetPairType.typeRef,
          "orderType"         -> ordTypeType,
          "price"             -> LONG,
          "amount"            -> LONG,
          "timestamp"         -> LONG,
          "expiration"        -> LONG,
          "matcherFee"        -> LONG,
          "matcherFeeAssetId" -> optionByteVector
        ) ++ proven,
        proofsEnabled
      )
    )
  }
  def buildExchangeTransactionType(proofsEnabled: Boolean) = CaseType(
    "ExchangeTransaction",
    addProofsIfNeeded(
      List(
        "buyOrder"       -> buildOrderType(proofsEnabled).typeRef,
        "sellOrder"      -> buildOrderType(proofsEnabled).typeRef,
        "price"          -> LONG,
        "amount"         -> LONG,
        "buyMatcherFee"  -> LONG,
        "sellMatcherFee" -> LONG
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTESTR, STRING)
  val dataEntryType              = CaseType("DataEntry", List("key" -> STRING, "value" -> dataEntryValueType))

  def buildDataTransactionType(proofsEnabled: Boolean) = CaseType(
    "DataTransaction",
    addProofsIfNeeded(List("data" -> LIST(dataEntryType.typeRef)) ++ header ++ proven, proofsEnabled)
  )

  def buildMassTransferTransactionType(proofsEnabled: Boolean) = CaseType(
    "MassTransferTransaction",
    addProofsIfNeeded(
      List(
        "feeAssetId"    -> optionByteVector,
        "assetId"       -> optionByteVector,
        "totalAmount"   -> LONG,
        "transfers"     -> listTransfers,
        "transferCount" -> LONG,
        "attachment"    -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildSetScriptTransactionType(proofsEnabled: Boolean) = CaseType(
    "SetScriptTransaction",
    addProofsIfNeeded(
      List(
        "script" -> optionByteVector
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildObsoleteTransactionTypes(proofsEnabled: Boolean): List[CaseType] = {
    List(genesisTransactionType, buildPaymentTransactionType(proofsEnabled))
  }

  def buildAssetSupportedTransactions(proofsEnabled: Boolean, v: StdLibVersion) = List(
    buildReissueTransactionType(proofsEnabled),
    buildBurnTransactionType(proofsEnabled),
    buildMassTransferTransactionType(proofsEnabled),
    buildExchangeTransactionType(proofsEnabled),
    buildTransferTransactionType(proofsEnabled),
    buildSetAssetScriptTransactionType(proofsEnabled)
  ) ++ (if (v == StdLibVersion.V3) List(buildInvokeScriptTransactionType(proofsEnabled)) else List.empty)

  def buildActiveTransactionTypes(proofsEnabled: Boolean, v: StdLibVersion): List[CaseType] = {
    buildAssetSupportedTransactions(proofsEnabled, v) ++
      List(
        buildIssueTransactionType(proofsEnabled),
        buildLeaseTransactionType(proofsEnabled),
        buildLeaseCancelTransactionType(proofsEnabled),
        buildCreateAliasTransactionType(proofsEnabled),
        buildSetScriptTransactionType(proofsEnabled),
        buildSponsorFeeTransactionType(proofsEnabled),
        buildDataTransactionType(proofsEnabled)
      )
  }

  def buildWavesTypes(proofsEnabled: Boolean, v: StdLibVersion): Seq[DefinedType] = {

    val activeTxTypes                    = buildActiveTransactionTypes(proofsEnabled, v)
    val obsoleteTxTypes                  = buildObsoleteTransactionTypes(proofsEnabled)
    val transactionsCommonType           = UnionType("Transaction", activeTxTypes.map(_.typeRef))
    val transactionTypes: List[CaseType] = obsoleteTxTypes ++ activeTxTypes

    Seq(
      addressType,
      aliasType,
      transfer,
      assetPairType,
      dataEntryType,
      buildOrderType(proofsEnabled),
      transactionsCommonType
    ) ++ transactionTypes
  }
}

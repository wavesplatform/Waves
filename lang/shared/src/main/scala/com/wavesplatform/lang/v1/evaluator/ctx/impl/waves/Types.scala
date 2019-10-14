package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.Types._

object Types {

  lazy val addressType        = CASETYPEREF("Address", List("bytes" -> BYTESTR))
  lazy val aliasType          = CASETYPEREF("Alias", List("alias" -> STRING))
  lazy val addressOrAliasType = UNION(addressType, aliasType)

  val assetType = CASETYPEREF(
    "Asset",
    List(
      "id"              -> BYTESTR,
      "quantity"        -> LONG,
      "decimals"        -> LONG,
      "issuer"          -> addressType,
      "issuerPublicKey" -> BYTESTR,
      "reissuable"      -> BOOLEAN,
      "scripted"        -> BOOLEAN,
      "sponsored"       -> BOOLEAN
    )
  )

  val blockInfo = CASETYPEREF(
    "BlockInfo",
    List(
      "timestamp"           -> LONG,
      "height"              -> LONG,
      "baseTarget"          -> LONG,
      "generationSignature" -> BYTESTR,
      "generator"           -> addressType,
      "generatorPublicKey"  -> BYTESTR
    )
  )

  val optionAsset = UNION(assetType, UNIT)

  val transfer = CASETYPEREF("Transfer", List("recipient" -> addressOrAliasType, "amount" -> LONG))

  val optionAddress = UNION(addressType, UNIT)
  val listTransfers = LIST(transfer)
  val paymentType   = CASETYPEREF("AttachedPayment", List("assetId" -> optionByteVector, "amount" -> LONG))

  val optionPayment = UNION(paymentType, UNIT)

  lazy val verifierInput = UNION.create(
    buildOrderType(true) :: buildActiveTransactionTypes(true, V3),
    Some("VerifierInput")
  )

  val invocationType =
    CASETYPEREF(
      "Invocation",
      List(
        "caller"          -> addressType,
        "callerPublicKey" -> BYTESTR,
        "payment"         -> optionPayment,
        "transactionId"   -> BYTESTR,
        "fee"             -> LONG,
        "feeAssetId"      -> optionByteVector
      )
    )

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTESTR, STRING)

  val dataEntryType = CASETYPEREF("DataEntry", List("key" -> STRING, "value" -> dataEntryValueType))

  val writeSetType =
    CASETYPEREF(
      FieldNames.WriteSet,
      List(FieldNames.Data -> LIST(dataEntryType))
    )

  private val scriptTransfer =
    CASETYPEREF(
      FieldNames.ScriptTransfer,
      List("recipient" -> addressOrAliasType, "amount" -> LONG, "asset" -> optionByteVector)
    )

  val scriptTransferSetType =
    CASETYPEREF(
      FieldNames.TransferSet,
      List(FieldNames.Transfers -> LIST(scriptTransfer))
    )

  val scriptResultType =
    CASETYPEREF(
      FieldNames.ScriptResult,
      List(FieldNames.ScriptWriteSet -> writeSetType, FieldNames.ScriptTransferSet -> scriptTransferSetType)
    )

  val dAppTypes = List(
    writeSetType,
    paymentType,
    scriptTransfer,
    scriptTransferSetType,
    scriptResultType,
    invocationType,
    assetType,
    blockInfo
  )

  private val header = List(
    "id"        -> BYTESTR,
    "fee"       -> LONG,
    "timestamp" -> LONG,
    "version"   -> LONG,
  )
  private lazy val proven = List(
    "sender"          -> addressType,
    "senderPublicKey" -> BYTESTR,
    "bodyBytes"       -> BYTESTR
  )

  private val proofs = "proofs" -> listByteVector

  val genesisTransactionType = CASETYPEREF(
    "GenesisTransaction",
    List("amount" -> LONG, "recipient" -> addressOrAliasType) ++ header
  )

  def anyTransactionType(proofsEnabled: Boolean, version: StdLibVersion): UNION =
    UNION(
      buildObsoleteTransactionTypes(proofsEnabled) ++ buildActiveTransactionTypes(proofsEnabled, version)
    )

  def txByIdReturnType(proofsEnabled: Boolean, version: StdLibVersion): UNION =
    UNION.create(UNIT +: anyTransactionType(proofsEnabled, version).typeList)

  def buildTransferTransactionType(proofsEnabled: Boolean) = {
    CASETYPEREF(
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

  def buildIssueTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
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

  def buildInvokeScriptTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "InvokeScriptTransaction",
    addProofsIfNeeded(
      List(
        "dApp"       -> addressOrAliasType,
        "payment"    -> optionPayment,
        "feeAssetId" -> optionByteVector,
        "function"   -> STRING,
        "args"       -> LIST(UNION(LONG, STRING, BOOLEAN, BYTESTR))
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildReissueTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
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

  def buildBurnTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "BurnTransaction",
    addProofsIfNeeded(
      List(
        "quantity" -> LONG,
        "assetId"  -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildSetAssetScriptTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "SetAssetScriptTransaction",
    addProofsIfNeeded(
      List(
        "script"  -> optionByteVector,
        "assetId" -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildLeaseTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "LeaseTransaction",
    addProofsIfNeeded(
      List(
        "amount"    -> LONG,
        "recipient" -> addressOrAliasType,
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildLeaseCancelTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "LeaseCancelTransaction",
    addProofsIfNeeded(
      List(
        "leaseId" -> BYTESTR,
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildCreateAliasTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "CreateAliasTransaction",
    addProofsIfNeeded(
      List(
        "alias" -> STRING,
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildPaymentTransactionType(proofsEnabled: Boolean) = {
    CASETYPEREF(
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

  def buildSponsorFeeTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "SponsorFeeTransaction",
    addProofsIfNeeded(
      List(
        "assetId"              -> BYTESTR,
        "minSponsoredAssetFee" -> optionLong
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  val buyType  = CASETYPEREF("Buy", List.empty)
  val sellType = CASETYPEREF("Sell", List.empty)

  val ordTypeType = UNION(buyType, sellType)

  val assetPairType = CASETYPEREF("AssetPair", List("amountAsset" -> optionByteVector, "priceAsset" -> optionByteVector))

  def buildOrderType(proofsEnabled: Boolean) = {
    CASETYPEREF(
      "Order",
      addProofsIfNeeded(
        List(
          "id"                -> BYTESTR,
          "matcherPublicKey"  -> BYTESTR,
          "assetPair"         -> assetPairType,
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
  def buildExchangeTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "ExchangeTransaction",
    addProofsIfNeeded(
      List(
        "buyOrder"       -> buildOrderType(proofsEnabled),
        "sellOrder"      -> buildOrderType(proofsEnabled),
        "price"          -> LONG,
        "amount"         -> LONG,
        "buyMatcherFee"  -> LONG,
        "sellMatcherFee" -> LONG
      ) ++ header ++ proven,
      proofsEnabled
    )
  )


  def buildDataTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "DataTransaction",
    addProofsIfNeeded(List("data" -> LIST(dataEntryType)) ++ header ++ proven, proofsEnabled)
  )

  def buildMassTransferTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
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

  def buildSetScriptTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "SetScriptTransaction",
    addProofsIfNeeded(
      List(
        "script" -> optionByteVector
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildObsoleteTransactionTypes(proofsEnabled: Boolean): List[CASETYPEREF] = {
    List(genesisTransactionType, buildPaymentTransactionType(proofsEnabled))
  }

  def buildAssetSupportedTransactions(proofsEnabled: Boolean, v: StdLibVersion) =
    List(
      buildReissueTransactionType(proofsEnabled),
      buildBurnTransactionType(proofsEnabled),
      buildMassTransferTransactionType(proofsEnabled),
      buildExchangeTransactionType(proofsEnabled),
      buildTransferTransactionType(proofsEnabled),
      buildSetAssetScriptTransactionType(proofsEnabled)
    ) ++ (if (v == V3) List(buildInvokeScriptTransactionType(proofsEnabled)) else List.empty)

  def buildActiveTransactionTypes(proofsEnabled: Boolean, v: StdLibVersion): List[CASETYPEREF] = {
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

  def buildWavesTypes(proofsEnabled: Boolean, v: StdLibVersion): Seq[FINAL] = {
    val activeTxTypes                       = buildActiveTransactionTypes(proofsEnabled, v)
    val obsoleteTxTypes                     = buildObsoleteTransactionTypes(proofsEnabled)
    val transactionsCommonType              = UNION.create(activeTxTypes, Some("Transaction"))
    val transactionTypes: List[CASETYPEREF] = obsoleteTxTypes ++ activeTxTypes

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

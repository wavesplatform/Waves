package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments._

object Types {

  lazy val addressType        = CASETYPEREF("Address", List("bytes" -> BYTESTR))
  lazy val aliasType          = CASETYPEREF("Alias", List("alias" -> STRING))
  lazy val addressOrAliasType = UNION(addressType, aliasType)

  val blockHeader = CASETYPEREF(
    "BlockHeader",
    List(
      "timestamp"                      -> LONG,
      "version"                        -> LONG,
      "reference"                      -> BYTESTR,
      "generator"                      -> BYTESTR,
      "generatorPublicKey"             -> BYTESTR,
      "signature"                      -> BYTESTR,
      "baseTarget"                     -> LONG,
      "generationSignature"            -> BYTESTR,
      "transactionCount"               -> LONG,
      "featureVotes"                   -> LIST(LONG)
    )
  )

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
  val listPayment   = LIST(paymentType)

  def verifierInput(version: StdLibVersion) =
    UNION.create(
    buildOrderType(true) :: buildActiveTransactionTypes(proofsEnabled = true, v = version),
    Some("VerifierInput")
  )

  def invocationType(v: StdLibVersion) =
    CASETYPEREF(
      "Invocation",
      payments(v.supportsMultiPayment) ::
      List(
        "caller"          -> addressType,
        "callerPublicKey" -> BYTESTR,
        "transactionId"   -> BYTESTR,
        "fee"             -> LONG,
        "feeAssetId"      -> optionByteVector
      )
    )

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTESTR, STRING)

  val genericDataEntry =
    CASETYPEREF(FieldNames.DataEntry, List("key" -> STRING, "value" -> dataEntryValueType))

  private def buildTypedEntry(name: String, valueType: REAL) =
    CASETYPEREF(name, List("key" -> STRING, "value" -> valueType))

  val booleanDataEntry: CASETYPEREF = buildTypedEntry(FieldNames.BooleanEntry, BOOLEAN)
  val stringDataEntry: CASETYPEREF  = buildTypedEntry(FieldNames.StringEntry, STRING)
  val binaryDataEntry: CASETYPEREF  = buildTypedEntry(FieldNames.BinaryEntry, BYTESTR)
  val intDataEntry: CASETYPEREF     = buildTypedEntry(FieldNames.IntegerEntry, LONG)
  val deleteDataEntry: CASETYPEREF  = CASETYPEREF(FieldNames.DeleteEntry, List("key" -> STRING))

  private val typedDataEntries =
    List(booleanDataEntry, stringDataEntry, binaryDataEntry, intDataEntry)

  def commonDataEntryType(v: StdLibVersion): FINAL =
    if (v >= V4) UNION(typedDataEntries) else genericDataEntry

  val writeSetType =
    CASETYPEREF(
      FieldNames.WriteSet,
      List(FieldNames.Data -> LIST(commonDataEntryType(V3)))
    )

  val scriptTransfer =
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

  val issueScriptType = CASETYPEREF(FieldNames.IssueScript, Nil, true)

  val issueActionType =
    CASETYPEREF(
      FieldNames.Issue,
      List(
        FieldNames.IssueScript -> UNION(issueScriptType, UNIT),
        FieldNames.IssueDecimals -> LONG,
        FieldNames.IssueDescription -> STRING,
        FieldNames.IssueIsReissuable -> BOOLEAN,
        FieldNames.IssueName -> STRING,
        FieldNames.IssueQuantity -> LONG,
        FieldNames.IssueNonce -> LONG,
      )
    )

  val reissueActionType =
    CASETYPEREF(
      FieldNames.Reissue,
      List(
        FieldNames.ReissueAssetId -> BYTESTR,
        FieldNames.ReissueIsReissuable -> BOOLEAN,
        FieldNames.ReissueQuantity -> LONG
      )
    )

  val burnActionType =
    CASETYPEREF(
      FieldNames.Burn,
      List(
        FieldNames.BurnAssetId  -> BYTESTR,
        FieldNames.BurnQuantity -> LONG
      )
    )

  private val callableV3Results =
    List(writeSetType, scriptTransferSetType, scriptResultType)

  private val callableV4Actions =
    List(issueActionType, reissueActionType, burnActionType)

  private def callableTypes(version: StdLibVersion) =
    if (version == V3) callableV3Results
    else if (version >= V4) callableV4Actions
    else Nil

  def dAppTypes(version: StdLibVersion): List[CASETYPEREF] =
    List(
      paymentType,
      scriptTransfer,
      invocationType(version),
      assetType,
      blockInfo
    ) ::: callableTypes(version)

  private val callableV3ReturnType =
    UNION(callableV3Results: _*)

  private val callableV4ReturnType =
    LIST(UNION.create(commonDataEntryType(V4) :: deleteDataEntry :: scriptTransfer :: callableV4Actions))

  def callableReturnType(v: StdLibVersion): Either[ExecutionError, FINAL] =
    v match {
      case V3 => Right(callableV3ReturnType)
      case V4 => Right(callableV4ReturnType)
      case v  => Left(s"DApp is not supported for V$v")
    }

  private def payments(multiPaymentAllowed: Boolean) =
    if (multiPaymentAllowed) "payments" -> listPayment
    else "payment" -> optionPayment

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

  def buildTransferTransactionType(proofsEnabled: Boolean, version: StdLibVersion) =
    CASETYPEREF(
      "TransferTransaction",
      addProofsIfNeeded(
        List(
          "feeAssetId" -> optionByteVector,
          "amount"     -> LONG,
          "assetId"    -> optionByteVector,
          "recipient"  -> addressOrAliasType,
        ) ++ header ++ proven :+ buildAttachmentType(version),
        proofsEnabled
      )
    )

  val genericAttachmentType: UNION =
    UNION(BYTESTR, LONG, BOOLEAN, BYTESTR, UNIT)

  private def buildAttachmentType(version: StdLibVersion) =
    "attachment" -> (if (version >= V4) genericAttachmentType else BYTESTR)

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

  def buildInvokeScriptTransactionType(proofsEnabled: Boolean, version: StdLibVersion) =
    CASETYPEREF(
      "InvokeScriptTransaction",
      addProofsIfNeeded(
        List(
          "dApp"       -> addressOrAliasType,
          "feeAssetId" -> optionByteVector,
          "function"   -> STRING,
          "args"       -> LIST(UNION(LONG, STRING, BOOLEAN, BYTESTR))
        ) ++ header ++ proven :+ payments(version.supportsMultiPayment),
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

  val buyType  = CASETYPEREF("Buy", List.empty, true)
  val sellType = CASETYPEREF("Sell", List.empty, true)

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

  def buildUpdateAssetInfoTransactionType(proofsEnabled: Boolean) =
    CASETYPEREF(
      "UpdateAssetInfoTransaction",
      addProofsIfNeeded(
        List(
          "assetId"     -> BYTESTR,
          "name"        -> STRING,
          "description" -> STRING
        ) ++ header ++ proven,
        proofsEnabled
      )
    )

  def buildDataTransactionType(proofsEnabled: Boolean, v: StdLibVersion) =
    CASETYPEREF(
      "DataTransaction",
      addProofsIfNeeded(List("data" -> LIST(commonDataEntryType(v))) ++ header ++ proven, proofsEnabled)
    )

  def buildMassTransferTransactionType(proofsEnabled: Boolean, version: StdLibVersion) =
    CASETYPEREF(
      "MassTransferTransaction",
      addProofsIfNeeded(
        List(
          "feeAssetId"    -> optionByteVector,
          "assetId"       -> optionByteVector,
          "totalAmount"   -> LONG,
          "transfers"     -> listTransfers,
          "transferCount" -> LONG,
        ) ++ header ++ proven :+ buildAttachmentType(version),
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

  def buildAssetSupportedTransactions(proofsEnabled: Boolean, v: StdLibVersion): List[CASETYPEREF] =
    List(
      buildReissueTransactionType(proofsEnabled),
      buildBurnTransactionType(proofsEnabled),
      buildMassTransferTransactionType(proofsEnabled, v),
      buildExchangeTransactionType(proofsEnabled),
      buildTransferTransactionType(proofsEnabled, v),
      buildSetAssetScriptTransactionType(proofsEnabled)
    ) ++ (if (v >= V3) List(buildInvokeScriptTransactionType(proofsEnabled, v)) else List.empty) ++
         (if (v >= V4) List(buildUpdateAssetInfoTransactionType(proofsEnabled)) else List.empty)

  def buildActiveTransactionTypes(proofsEnabled: Boolean, v: StdLibVersion): List[CASETYPEREF] = {
    buildAssetSupportedTransactions(proofsEnabled, v) ++
      List(
        buildIssueTransactionType(proofsEnabled),
        buildLeaseTransactionType(proofsEnabled),
        buildLeaseCancelTransactionType(proofsEnabled),
        buildCreateAliasTransactionType(proofsEnabled),
        buildSetScriptTransactionType(proofsEnabled),
        buildSponsorFeeTransactionType(proofsEnabled),
        buildDataTransactionType(proofsEnabled, v)
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
      buildOrderType(proofsEnabled),
      transactionsCommonType
    ) ++
      transactionTypes ++
      (if (v >= V4) blockHeader :: deleteDataEntry :: typedDataEntries else Seq(genericDataEntry))
  }
}

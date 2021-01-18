package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments._

object Types {

  lazy val addressType        = CASETYPEREF("Address", List("bytes" -> BYTESTR))
  lazy val aliasType          = CASETYPEREF("Alias", List("alias" -> STRING))
  lazy val addressOrAliasType = UNION(addressType, aliasType)

  def assetType(version: StdLibVersion) = {
    val versionDepFields =
      if (version >= V4)
        List("minSponsoredFee" -> optionLong, "name" -> STRING, "description" -> STRING)
      else
        List("sponsored" -> BOOLEAN)

    CASETYPEREF(
      "Asset",
      List(
        "id"              -> BYTESTR,
        "quantity"        -> LONG,
        "decimals"        -> LONG,
        "issuer"          -> addressType,
        "issuerPublicKey" -> BYTESTR,
        "reissuable"      -> BOOLEAN,
        "scripted"        -> BOOLEAN
      ) ++ versionDepFields
    )
  }

  def blockInfo(version: StdLibVersion) = CASETYPEREF(
    "BlockInfo",
    List(
      "timestamp"           -> LONG,
      "height"              -> LONG,
      "baseTarget"          -> LONG,
      "generationSignature" -> BYTESTR,
      "generator"           -> addressType,
      "generatorPublicKey"  -> BYTESTR
    ) ::: (if (version >= V4) List("vrf" -> optionByteVector) else Nil)
  )

  def optionAsset(version: StdLibVersion) =
    UNION(assetType(version), UNIT)

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
    List(booleanDataEntry, stringDataEntry, binaryDataEntry, intDataEntry, deleteDataEntry)

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
        FieldNames.IssueName         -> STRING,
        FieldNames.IssueDescription  -> STRING,
        FieldNames.IssueQuantity     -> LONG,
        FieldNames.IssueDecimals     -> LONG,
        FieldNames.IssueIsReissuable -> BOOLEAN,
        FieldNames.IssueScriptField  -> UNION(issueScriptType, UNIT),
        FieldNames.IssueNonce        -> LONG
      ),
      hideConstructor = true
    )

  val reissueActionType =
    CASETYPEREF(
      FieldNames.Reissue,
      List(
        FieldNames.ReissueAssetId      -> BYTESTR,
        FieldNames.ReissueQuantity     -> LONG,
        FieldNames.ReissueIsReissuable -> BOOLEAN
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

  val sponsorFeeActionType =
    CASETYPEREF(
      FieldNames.SponsorFee,
      List(
        FieldNames.SponsorFeeAssetId -> BYTESTR,
        FieldNames.SponsorFeeMinFee  -> optionLong
      )
    )

  val leaseActionType =
    CASETYPEREF(
      FieldNames.Lease,
      List(
        FieldNames.LeaseRecipient -> addressOrAliasType,
        FieldNames.LeaseAmount    -> LONG,
        FieldNames.LeaseNonce     -> LONG
      )
    )

  val leaseCancelActionType =
    CASETYPEREF(
      FieldNames.LeaseCancel,
      List(
        FieldNames.LeaseId -> BYTESTR
      )
    )

  private val callableV3Results =
    List(writeSetType, scriptTransferSetType, scriptResultType)

  private val callableV4Actions =
    List(issueActionType, reissueActionType, burnActionType, sponsorFeeActionType)

  private val callableV5Actions =
    List(leaseActionType, leaseCancelActionType)

  private def callableTypes(version: StdLibVersion): List[CASETYPEREF] =
    version match {
      case V3 => callableV3Results
      case V4 => callableV4Actions
      case V5 => callableV4Actions ::: callableV5Actions
      case _  => Nil
    }

  def dAppTypes(version: StdLibVersion): List[CASETYPEREF] =
    List(
      paymentType,
      scriptTransfer,
      invocationType(version),
      assetType(version),
      blockInfo(version)
    ) ::: callableTypes(version)

  private val callableV3ReturnType =
    UNION(callableV3Results: _*)

  private val callableV4ReturnType = {
    val actions = LIST(UNION.create(commonDataEntryType(V4) :: deleteDataEntry :: scriptTransfer :: callableV4Actions))
    actions
  }

  private val callableV5ReturnType = {
    val actions = LIST(UNION.create(commonDataEntryType(V5) :: scriptTransfer :: callableV4Actions ::: callableV5Actions))
    UNION(actions, TUPLE(List(actions, ANY)))
  }

  def callableReturnType(v: StdLibVersion): Either[ExecutionError, FINAL] =
    v match {
      case V3 => Right(callableV3ReturnType)
      case V4 => Right(callableV4ReturnType)
      case V5 => Right(callableV5ReturnType)
      case v  => Left(s"DApp is not supported for $v")
    }

  private def payments(multiPaymentAllowed: Boolean) =
    if (multiPaymentAllowed) "payments" -> listPayment
    else "payment"                      -> optionPayment

  private val header = List(
    "id"        -> BYTESTR,
    "fee"       -> LONG,
    "timestamp" -> LONG,
    "version"   -> LONG
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

  def anyTransactionType(proofsEnabled: Boolean, version: StdLibVersion): FINAL =
    UNION(
      buildObsoleteTransactionTypes(proofsEnabled) ++ buildActiveTransactionTypes(proofsEnabled, version)
    )

  def txByIdReturnType(proofsEnabled: Boolean, version: StdLibVersion): FINAL =
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
          "attachment" -> BYTESTR
        ) ++ header ++ proven,
        proofsEnabled
      )
    )

  def addProofsIfNeeded(commonFields: List[(String, FINAL)], proofsEnabled: Boolean): List[(String, FINAL)] = {
    if (proofsEnabled) commonFields :+ proofs
    else commonFields
  }

  def buildIssueTransactionType(proofsEnabled: Boolean, version: StdLibVersion) = CASETYPEREF(
    "IssueTransaction",
    addProofsIfNeeded(
      List(
        "quantity"    -> LONG,
        "name"        -> (if (version >= V4) STRING else BYTESTR),
        "description" -> (if (version >= V4) STRING else BYTESTR),
        "reissuable"  -> BOOLEAN,
        "decimals"    -> LONG,
        "script"      -> optionByteVector
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildInvokeScriptTransactionType(proofsEnabled: Boolean, version: StdLibVersion) = {
    val argTypes = UNION(LONG, STRING, BOOLEAN, BYTESTR, LIST(UNION(LONG, STRING, BOOLEAN, BYTESTR)))
    CASETYPEREF(
      "InvokeScriptTransaction",
      addProofsIfNeeded(
        List(
          "dApp"       -> addressOrAliasType,
          "feeAssetId" -> optionByteVector,
          "function"   -> STRING,
          "args"       -> LIST(argTypes)
        ) ++ header ++ proven :+ payments(version.supportsMultiPayment),
        proofsEnabled
      )
    )
  }

  def buildReissueTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "ReissueTransaction",
    addProofsIfNeeded(
      List(
        "quantity"   -> LONG,
        "assetId"    -> BYTESTR,
        "reissuable" -> BOOLEAN
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
        "recipient" -> addressOrAliasType
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildLeaseCancelTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "LeaseCancelTransaction",
    addProofsIfNeeded(
      List(
        "leaseId" -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildCreateAliasTransactionType(proofsEnabled: Boolean) = CASETYPEREF(
    "CreateAliasTransaction",
    addProofsIfNeeded(
      List(
        "alias" -> STRING
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
        buildIssueTransactionType(proofsEnabled, v),
        buildLeaseTransactionType(proofsEnabled),
        buildLeaseCancelTransactionType(proofsEnabled),
        buildCreateAliasTransactionType(proofsEnabled),
        buildSetScriptTransactionType(proofsEnabled),
        buildSponsorFeeTransactionType(proofsEnabled),
        buildDataTransactionType(proofsEnabled, v)
      )
  }

  val balanceDetailsType = CASETYPEREF(
    "BalanceDetails",
    List(
      "available"  -> LONG,
      "regular"    -> LONG,
      "generating" -> LONG,
      "effective"  -> LONG
    )
  )

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
      (if (v >= V4) balanceDetailsType :: typedDataEntries else Seq(genericDataEntry))
  }
}

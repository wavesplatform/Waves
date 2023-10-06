package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments._

object Types {

  lazy val addressType: CASETYPEREF      = CASETYPEREF("Address", List("bytes" -> BYTESTR))
  lazy val aliasType: CASETYPEREF        = CASETYPEREF("Alias", List("alias" -> STRING))
  lazy val addressOrAliasType: UNIONLIKE = UNION(addressType, aliasType)

  def assetType(version: StdLibVersion): CASETYPEREF = {
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

  def blockInfo(version: StdLibVersion): CASETYPEREF = CASETYPEREF(
    "BlockInfo",
    List(
      "timestamp"           -> LONG,
      "height"              -> LONG,
      "baseTarget"          -> LONG,
      "generationSignature" -> BYTESTR,
      "generator"           -> addressType,
      "generatorPublicKey"  -> BYTESTR
    ) ::: (if (version >= V4) List("vrf" -> optionByteVector) else Nil) :::
      (if (version >= V7) List("rewards" -> LIST(TUPLE(List(addressType, LONG)))) else Nil)
  )

  val transfer: CASETYPEREF = CASETYPEREF("Transfer", List("recipient" -> addressOrAliasType, "amount" -> LONG))

  val optionAddress: UNIONLIKE = UNION(addressType, UNIT)
  val listTransfers: LIST      = LIST(transfer)
  val paymentType: CASETYPEREF = CASETYPEREF("AttachedPayment", List("assetId" -> optionByteVector, "amount" -> LONG))

  val optionPayment: UNIONLIKE = UNION(paymentType, UNIT)
  val listPayment: LIST        = LIST(paymentType)

  def verifierInput(version: StdLibVersion): UNIONLIKE =
    UNION.create(
      buildOrderType(proofsEnabled = true, version) :: buildActiveTransactionTypes(proofsEnabled = true, v = version),
      Some("VerifierInput")
    )

  def invocationType(v: StdLibVersion): CASETYPEREF =
    CASETYPEREF(
      "Invocation",
      payments(v.supportsMultiPayment) ::
        List(
          "caller"          -> addressType,
          "callerPublicKey" -> BYTESTR,
          "transactionId"   -> BYTESTR,
          "fee"             -> LONG,
          "feeAssetId"      -> optionByteVector
        ) :::
        (if (v >= V5)
           List(
             "originCaller"          -> addressType,
             "originCallerPublicKey" -> BYTESTR
           )
         else Nil)
    )

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTESTR, STRING)

  val genericDataEntry: CASETYPEREF =
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

  val writeSetType: CASETYPEREF =
    CASETYPEREF(
      FieldNames.WriteSet,
      List(FieldNames.Data -> LIST(commonDataEntryType(V3)))
    )

  val scriptTransfer: CASETYPEREF =
    CASETYPEREF(
      FieldNames.ScriptTransfer,
      List("recipient" -> addressOrAliasType, "amount" -> LONG, "asset" -> optionByteVector)
    )

  val scriptTransferSetType: CASETYPEREF =
    CASETYPEREF(
      FieldNames.TransferSet,
      List(FieldNames.Transfers -> LIST(scriptTransfer))
    )

  val scriptResultType: CASETYPEREF =
    CASETYPEREF(
      FieldNames.ScriptResult,
      List(FieldNames.ScriptWriteSet -> writeSetType, FieldNames.ScriptTransferSet -> scriptTransferSetType)
    )

  val issueScriptType: CASETYPEREF = CASETYPEREF(FieldNames.IssueScript, Nil, hideConstructor = true)

  val issueActionType: CASETYPEREF =
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

  val reissueActionType: CASETYPEREF =
    CASETYPEREF(
      FieldNames.Reissue,
      List(
        FieldNames.ReissueAssetId      -> BYTESTR,
        FieldNames.ReissueQuantity     -> LONG,
        FieldNames.ReissueIsReissuable -> BOOLEAN
      )
    )

  val burnActionType: CASETYPEREF =
    CASETYPEREF(
      FieldNames.Burn,
      List(
        FieldNames.BurnAssetId  -> BYTESTR,
        FieldNames.BurnQuantity -> LONG
      )
    )

  val sponsorFeeActionType: CASETYPEREF =
    CASETYPEREF(
      FieldNames.SponsorFee,
      List(
        FieldNames.SponsorFeeAssetId -> BYTESTR,
        FieldNames.SponsorFeeMinFee  -> optionLong
      )
    )

  val leaseActionType: CASETYPEREF =
    CASETYPEREF(
      FieldNames.Lease,
      List(
        FieldNames.LeaseRecipient -> addressOrAliasType,
        FieldNames.LeaseAmount    -> LONG,
        FieldNames.LeaseNonce     -> LONG
      )
    )

  val leaseCancelActionType: CASETYPEREF =
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
      case V3           => callableV3Results
      case V4           => callableV4Actions
      case v if v >= V5 => callableV4Actions ::: callableV5Actions
      case _            => Nil
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
    UNION(callableV3Results*)

  private val callableV4ReturnType = {
    val actions = LIST(UNION.create(commonDataEntryType(V4) :: deleteDataEntry :: scriptTransfer :: callableV4Actions))
    actions
  }

  private val callableV5ReturnType = {
    val actions = LIST(UNION.create(commonDataEntryType(V5) :: scriptTransfer :: callableV4Actions ::: callableV5Actions))
    UNION(actions, TUPLE(List(actions, ANY)))
  }

  def callableReturnType(v: StdLibVersion): Either[String, FINAL] =
    v match {
      case V1 | V2 => Left(s"DApp is not supported for $v")
      case V3      => Right(callableV3ReturnType)
      case V4      => Right(callableV4ReturnType)
      case _       => Right(callableV5ReturnType)
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

  val genesisTransactionType: CASETYPEREF = CASETYPEREF(
    "GenesisTransaction",
    List("amount" -> LONG, "recipient" -> addressOrAliasType) ++ header
  )

  def anyTransactionType(proofsEnabled: Boolean, version: StdLibVersion): FINAL =
    UNION(
      buildObsoleteTransactionTypes(proofsEnabled, version) ++ buildActiveTransactionTypes(proofsEnabled, version)
    )

  def txByIdReturnType(proofsEnabled: Boolean, version: StdLibVersion): FINAL =
    UNION.create(UNIT +: anyTransactionType(proofsEnabled, version).typeList)

  def buildTransferTransactionType(proofsEnabled: Boolean): CASETYPEREF =
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

  def buildIssueTransactionType(proofsEnabled: Boolean, version: StdLibVersion): CASETYPEREF = CASETYPEREF(
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

  def buildInvokeScriptTransactionType(proofsEnabled: Boolean, version: StdLibVersion): CASETYPEREF = {
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

  def buildInvokeExpressionTransactionType(proofsEnabled: Boolean): CASETYPEREF = {
    CASETYPEREF(
      "InvokeExpressionTransaction",
      addProofsIfNeeded(
        List(
          "expression" -> BYTESTR,
          "feeAssetId" -> optionByteVector
        ) ++ header ++ proven,
        proofsEnabled
      )
    )
  }

  def buildReissueTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
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

  def buildBurnTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
    "BurnTransaction",
    addProofsIfNeeded(
      List(
        "quantity" -> LONG,
        "assetId"  -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildSetAssetScriptTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
    "SetAssetScriptTransaction",
    addProofsIfNeeded(
      List(
        "script"  -> optionByteVector,
        "assetId" -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildLeaseTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
    "LeaseTransaction",
    addProofsIfNeeded(
      List(
        "amount"    -> LONG,
        "recipient" -> addressOrAliasType
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildLeaseCancelTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
    "LeaseCancelTransaction",
    addProofsIfNeeded(
      List(
        "leaseId" -> BYTESTR
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildCreateAliasTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
    "CreateAliasTransaction",
    addProofsIfNeeded(
      List(
        "alias" -> STRING
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildPaymentTransactionType(proofsEnabled: Boolean): CASETYPEREF = {
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

  def buildSponsorFeeTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
    "SponsorFeeTransaction",
    addProofsIfNeeded(
      List(
        "assetId"              -> BYTESTR,
        "minSponsoredAssetFee" -> optionLong
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  val buyType: CASETYPEREF  = CASETYPEREF("Buy", List.empty, hideConstructor = true)
  val sellType: CASETYPEREF = CASETYPEREF("Sell", List.empty, hideConstructor = true)

  val ordTypeType: UNIONLIKE = UNION(buyType, sellType)

  val assetPairType: CASETYPEREF = CASETYPEREF("AssetPair", List("amountAsset" -> optionByteVector, "priceAsset" -> optionByteVector))

  def buildOrderType(proofsEnabled: Boolean, version: StdLibVersion): CASETYPEREF = {
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
        ) ++ proven ++ (if (version >= V8) List("attachment" -> optionByteVector) else Nil),
        proofsEnabled
      )
    )
  }

  def buildExchangeTransactionType(proofsEnabled: Boolean, version: StdLibVersion): CASETYPEREF = CASETYPEREF(
    "ExchangeTransaction",
    addProofsIfNeeded(
      List(
        "buyOrder"       -> buildOrderType(proofsEnabled, version),
        "sellOrder"      -> buildOrderType(proofsEnabled, version),
        "price"          -> LONG,
        "amount"         -> LONG,
        "buyMatcherFee"  -> LONG,
        "sellMatcherFee" -> LONG
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildUpdateAssetInfoTransactionType(proofsEnabled: Boolean): CASETYPEREF =
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

  def buildDataTransactionType(proofsEnabled: Boolean, v: StdLibVersion): CASETYPEREF =
    CASETYPEREF(
      "DataTransaction",
      addProofsIfNeeded(List("data" -> LIST(commonDataEntryType(v))) ++ header ++ proven, proofsEnabled)
    )

  def buildMassTransferTransactionType(proofsEnabled: Boolean, version: StdLibVersion): CASETYPEREF =
    CASETYPEREF(
      "MassTransferTransaction",
      addProofsIfNeeded(
        (if (version < V5) List("feeAssetId" -> optionByteVector) else Nil) :::
          List(
            "assetId"       -> optionByteVector,
            "totalAmount"   -> LONG,
            "transfers"     -> listTransfers,
            "transferCount" -> LONG,
            "attachment"    -> BYTESTR
          ) ::: header ::: proven,
        proofsEnabled
      )
    )

  def buildSetScriptTransactionType(proofsEnabled: Boolean): CASETYPEREF = CASETYPEREF(
    "SetScriptTransaction",
    addProofsIfNeeded(
      List(
        "script" -> optionByteVector
      ) ++ header ++ proven,
      proofsEnabled
    )
  )

  def buildObsoleteTransactionTypes(proofsEnabled: Boolean, v: StdLibVersion): List[CASETYPEREF] = {
    val types = List(genesisTransactionType, buildPaymentTransactionType(proofsEnabled))
    if (v >= V6) types.map(_.copy(hideConstructor = true)) else types
  }

  def buildAssetSupportedTransactions(proofsEnabled: Boolean, v: StdLibVersion): List[CASETYPEREF] = {
    val types = List(
      buildReissueTransactionType(proofsEnabled),
      buildBurnTransactionType(proofsEnabled),
      buildMassTransferTransactionType(proofsEnabled, v),
      buildExchangeTransactionType(proofsEnabled, v),
      buildTransferTransactionType(proofsEnabled),
      buildSetAssetScriptTransactionType(proofsEnabled)
    ) ++ (if (v >= V3) List(buildInvokeScriptTransactionType(proofsEnabled, v)) else List.empty) ++
      (if (v >= V4) List(buildUpdateAssetInfoTransactionType(proofsEnabled)) else List.empty) ++
      (if (v >= V6) List(buildInvokeExpressionTransactionType(proofsEnabled)) else List.empty)

    if (v >= V6) types.map(_.copy(hideConstructor = true)) else types
  }

  def buildActiveTransactionTypes(proofsEnabled: Boolean, v: StdLibVersion): List[CASETYPEREF] = {
    val types = buildAssetSupportedTransactions(proofsEnabled, v) ++
      List(
        buildIssueTransactionType(proofsEnabled, v),
        buildLeaseTransactionType(proofsEnabled),
        buildLeaseCancelTransactionType(proofsEnabled),
        buildCreateAliasTransactionType(proofsEnabled),
        buildSetScriptTransactionType(proofsEnabled),
        buildSponsorFeeTransactionType(proofsEnabled),
        buildDataTransactionType(proofsEnabled, v)
      )
    if (v >= V6) types.map(_.copy(hideConstructor = true)) else types
  }

  val balanceDetailsType: CASETYPEREF = CASETYPEREF(
    "BalanceDetails",
    List(
      "available"  -> LONG,
      "regular"    -> LONG,
      "generating" -> LONG,
      "effective"  -> LONG
    )
  )

  def buildWavesTypes(proofsEnabled: Boolean, stdLibVersion: StdLibVersion): Seq[FINAL] = {
    val activeTxTypes                       = buildActiveTransactionTypes(proofsEnabled, stdLibVersion)
    val obsoleteTxTypes                     = buildObsoleteTransactionTypes(proofsEnabled, stdLibVersion)
    val transactionsCommonType              = UNION.create(activeTxTypes, Some("Transaction"))
    val transactionTypes: List[CASETYPEREF] = obsoleteTxTypes ++ activeTxTypes

    Seq(
      addressType,
      aliasType,
      transfer,
      assetPairType,
      buildOrderType(proofsEnabled, stdLibVersion),
      transactionsCommonType
    ) ++
      transactionTypes ++
      (if (stdLibVersion >= V4) balanceDetailsType :: typedDataEntries else Seq(genericDataEntry))
  }
}

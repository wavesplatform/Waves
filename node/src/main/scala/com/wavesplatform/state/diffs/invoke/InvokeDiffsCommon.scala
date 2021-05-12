package com.wavesplatform.state.diffs.invoke

import cats.implicits._
import com.google.common.base.Throwables
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.features.InvokeScriptSelfPaymentPolicyProvider._
import com.wavesplatform.features.ScriptTransferValidationProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Tx.{BurnPseudoTx, ReissuePseudoTx, ScriptTransfer, SponsorFeePseudoTx}
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.settings.Constants
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.DiffsCommon
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.AssetVerifierTrace.AssetContext
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, TracedResult}
import com.wavesplatform.transaction.validation.impl.SponsorFeeTxValidator
import com.wavesplatform.utils._
import shapeless.Coproduct

import scala.util.{Failure, Right, Success, Try}

object InvokeDiffsCommon {
  def calcFee(
      blockchain: Blockchain,
      tx: InvokeScriptTransaction
  ): Either[ValidationError, (Long, Map[Address, Portfolio])] = {
    tx.assetFee._1 match {
      case Waves => Right((tx.fee, Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty))))
      case asset @ IssuedAsset(_) =>
        for {
          assetInfo <- blockchain
            .assetDescription(asset)
            .toRight(GenericError(s"Asset $asset does not exist, cannot be used to pay fees"))
          wavesFee <- Either.cond(
            assetInfo.sponsorship > 0,
            Sponsorship.toWaves(tx.fee, assetInfo.sponsorship),
            GenericError(s"Asset $asset is not sponsored, cannot be used to pay fees")
          )
        } yield wavesFee ->
          (Map(tx.sender.toAddress         -> Portfolio(0, LeaseBalance.empty, Map(asset         -> -tx.fee))) |+|
            Map(assetInfo.issuer.toAddress -> Portfolio(-wavesFee, LeaseBalance.empty, Map(asset -> tx.fee))))
    }
  }

  def getInvocationComplexity(
      blockchain: Blockchain,
      tx: InvokeScriptTransaction,
      callableComplexities: Map[Int, Map[String, Long]],
      dAppAddress: Address
  ): Either[ValidationError, Long] = {
    for {
      complexitiesByCallable <- callableComplexities.get(blockchain.estimator.version).toRight {
        GenericError(s"Cannot find complexity storage, address = $dAppAddress, estimator version = ${blockchain.estimator.version}")
      }
      complexity <- complexitiesByCallable.get(tx.funcCall.function.funcName).toRight {
        GenericError(s"Cannot find callable function `${tx.funcCall.function.funcName}`, address = $dAppAddress`")
      }
    } yield complexity
  }

  def processActions(
      actions: List[CallableAction],
      version: StdLibVersion,
      dAppAddress: Address,
      dAppPublicKey: PublicKey,
      feeInfo: (Long, Map[Address, Portfolio]),
      invocationComplexity: Long,
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      blockTime: Long,
      limitedExecution: Boolean = false
  ): TracedResult[ValidationError, Diff] = {
    val complexityLimit =
      if (limitedExecution) ContractLimits.FailFreeInvokeComplexity - invocationComplexity.toInt
      else Int.MaxValue

    val actionsByType  = actions.groupBy(a => if (classOf[DataOp].isAssignableFrom(a.getClass)) classOf[DataOp] else a.getClass).withDefaultValue(Nil)
    val transferList   = actionsByType(classOf[AssetTransfer]).asInstanceOf[List[AssetTransfer]]
    val issueList      = actionsByType(classOf[Issue]).asInstanceOf[List[Issue]]
    val reissueList    = actionsByType(classOf[Reissue]).asInstanceOf[List[Reissue]]
    val burnList       = actionsByType(classOf[Burn]).asInstanceOf[List[Burn]]
    val sponsorFeeList = actionsByType(classOf[SponsorFee]).asInstanceOf[List[SponsorFee]]
    val dataEntries    = actionsByType(classOf[DataOp]).asInstanceOf[List[DataOp]].map(dataItemToEntry)

    for {
      _ <- TracedResult(checkDataEntries(tx, dataEntries, version)).leftMap(FailedTransactionError.dAppExecution(_, invocationComplexity))
      _ <- TracedResult(
        Either.cond(
          actions.length - dataEntries.length <= ContractLimits.MaxCallableActionsAmount,
          (),
          FailedTransactionError.dAppExecution(
            s"Too many script actions: max: ${ContractLimits.MaxCallableActionsAmount}, actual: ${actions.length}",
            invocationComplexity
          )
        )
      )

      _ <- TracedResult(checkSelfPayments(dAppAddress, blockchain, tx, version, transferList))
        .leftMap(FailedTransactionError.dAppExecution(_, invocationComplexity))
      _ <- TracedResult(
        Either.cond(transferList.map(_.amount).forall(_ >= 0), (), FailedTransactionError.dAppExecution("Negative amount", invocationComplexity))
      )
      _ <- TracedResult(checkOverflow(transferList.map(_.amount))).leftMap(FailedTransactionError.dAppExecution(_, invocationComplexity))

      scriptsInvoked <- TracedResult {
        val stepLimit = ContractLimits.MaxComplexityByVersion(version)
        val stepsNumber =
          if (invocationComplexity % stepLimit == 0)
            invocationComplexity / stepLimit
          else
            invocationComplexity / stepLimit + 1
        val stepsInfo = if (stepsNumber > 1) s" with $stepsNumber invocation steps" else ""

        val smartAssetInvocations =
          tx.checkedAssets ++
            transferList.flatMap(_.assetId).map(IssuedAsset) ++
            reissueList.map(r => IssuedAsset(r.assetId)) ++
            burnList.map(b => IssuedAsset(b.assetId)) ++
            sponsorFeeList.map(sf => IssuedAsset(sf.assetId))
        val totalScriptsInvoked = smartAssetInvocations.count(blockchain.hasAssetScript) + (if (blockchain.hasAccountScript(tx.sender.toAddress)) 1
                                                                                            else 0)
        val minIssueFee       = issueList.count(i => !blockchain.isNFT(i)) * FeeConstants(IssueTransaction.typeId) * FeeUnit
        val dAppInvocationFee = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit * stepsNumber
        val minWaves          = totalScriptsInvoked * ScriptExtraFee + dAppInvocationFee + minIssueFee
        val txName            = Constants.TransactionNames(InvokeScriptTransaction.typeId)
        val assetName         = tx.assetFee._1.fold("WAVES")(_.id.toString)
        Either.cond(
          minWaves <= feeInfo._1,
          totalScriptsInvoked,
          FailedTransactionError.feeForActions(
            s"Fee in $assetName for $txName (${tx.assetFee._2} in $assetName)" +
              s" with $totalScriptsInvoked total scripts invoked$stepsInfo does not exceed minimal value of $minWaves WAVES.",
            invocationComplexity
          )
        )
      }

      paymentsAndFeeDiff = paymentsPart(tx, dAppAddress, feeInfo._2)

      compositeDiff <- foldActions(blockchain, blockTime, tx, dAppAddress, dAppPublicKey)(actions, paymentsAndFeeDiff, complexityLimit)
        .leftMap(_.addComplexity(invocationComplexity))

      transfers = compositeDiff.portfolios |+| feeInfo._2.view.mapValues(_.negate).toMap

      currentTxDiff         = compositeDiff.transactions(tx.id())
      currentTxDiffWithKeys = currentTxDiff.copy(affected = currentTxDiff.affected ++ transfers.keys ++ compositeDiff.accountData.keys)
      updatedTxDiff         = compositeDiff.transactions.concat(Map(tx.id() -> currentTxDiffWithKeys))

      isr = InvokeScriptResult(
        dataEntries,
        transferList.map { tr =>
          InvokeScriptResult.Payment(
            Address.fromBytes(tr.recipient.bytes.arr).explicitGet(),
            Asset.fromCompatId(tr.assetId),
            tr.amount
          )
        },
        issueList,
        reissueList,
        burnList,
        sponsorFeeList
      )

      resultDiff = compositeDiff.copy(
        transactions = updatedTxDiff,
        scriptsRun = scriptsInvoked + 1,
        scriptResults = Map(tx.id() -> isr),
        scriptsComplexity = invocationComplexity + compositeDiff.scriptsComplexity
      )
    } yield resultDiff
  }

  def paymentsPart(
      tx: InvokeScriptTransaction,
      dAppAddress: Address,
      feePart: Map[Address, Portfolio]
  ): Diff = {
    val payablePart = tx.payments
      .map {
        case InvokeScriptTransaction.Payment(amt, assetId) =>
          assetId match {
            case asset @ IssuedAsset(_) =>
              Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amt))) |+|
                Map(dAppAddress       -> Portfolio(0, LeaseBalance.empty, Map(asset -> amt)))
            case Waves =>
              Map(tx.sender.toAddress -> Portfolio(-amt, LeaseBalance.empty, Map.empty)) |+|
                Map(dAppAddress       -> Portfolio(amt, LeaseBalance.empty, Map.empty))
          }
      }
      .foldLeft(Map[Address, Portfolio]())(_ |+| _)

    Diff(tx = tx, portfolios = feePart |+| payablePart)
  }

  private def dataItemToEntry(item: DataOp): DataEntry[_] =
    item match {
      case DataItem.Bool(k, b) => BooleanDataEntry(k, b)
      case DataItem.Str(k, b)  => StringDataEntry(k, b)
      case DataItem.Lng(k, b)  => IntegerDataEntry(k, b)
      case DataItem.Bin(k, b)  => BinaryDataEntry(k, b)
      case DataItem.Delete(k)  => EmptyDataEntry(k)
    }

  private def checkSelfPayments(
      dAppAddress: Address,
      blockchain: Blockchain,
      tx: InvokeScriptTransaction,
      version: StdLibVersion,
      transfers: List[AssetTransfer]
  ): Either[String, Unit] =
    if (blockchain.disallowSelfPayment && version >= V4)
      if (tx.payments.nonEmpty && tx.sender.toAddress == dAppAddress)
        "DApp self-payment is forbidden since V4".asLeft[Unit]
      else if (transfers.exists(_.recipient.bytes == ByteStr(dAppAddress.bytes)))
        "DApp self-transfer is forbidden since V4".asLeft[Unit]
      else
        ().asRight[String]
    else
      ().asRight[String]

  private def checkOverflow(dataList: Iterable[Long]): Either[String, Unit] = {
    Try(dataList.foldLeft(0L)(Math.addExact))
      .fold(
        _ => "Attempt to transfer unavailable funds in contract payment".asLeft[Unit],
        _ => ().asRight[String]
      )
  }

  private[this] def checkDataEntries(
      tx: InvokeScriptTransaction,
      dataEntries: Seq[DataEntry[_]],
      stdLibVersion: StdLibVersion
  ): Either[String, Unit] =
    for {
      _ <- Either.cond(
        dataEntries.length <= ContractLimits.MaxWriteSetSize,
        (),
        s"WriteSet can't contain more than ${ContractLimits.MaxWriteSetSize} entries"
      )
      _ <- Either.cond(
        !tx.isProtobufVersion || dataEntries.forall(_.key.nonEmpty),
        (),
        s"Empty keys aren't allowed in tx version >= ${tx.protobufVersion}"
      )

      maxKeySize = ContractLimits.MaxKeySizeInBytesByVersion(stdLibVersion)
      _ <- dataEntries
        .collectFirst {
          Function.unlift { entry =>
            val length = entry.key.utf8Bytes.length
            if (length > maxKeySize)
              Some(s"Data entry key size = $length bytes must be less than $maxKeySize")
            else if (entry.key.isEmpty && stdLibVersion >= V4)
              Some(s"Data entry key should not be empty")
            else
              None
          }
        }
        .toLeft(())

      totalDataBytes = dataEntries.map(_.toBytes.length).sum
      _ <- Either.cond(
        totalDataBytes <= ContractLimits.MaxWriteSetSizeInBytes,
        (),
        s"WriteSet size can't exceed ${ContractLimits.MaxWriteSetSizeInBytes} bytes, actual: $totalDataBytes bytes"
      )
    } yield ()

  private def foldActions(
      sblockchain: Blockchain,
      blockTime: Long,
      tx: InvokeScriptTransaction,
      dAppAddress: Address,
      pk: PublicKey
  )(
      actions: List[CallableAction],
      paymentsDiff: Diff,
      remainingLimit: Int
  ): TracedResult[FailedTransactionError, Diff] =
    actions.foldLeft(TracedResult(paymentsDiff.asRight[FailedTransactionError])) { (diffAcc, action) =>
      diffAcc match {
        case TracedResult(Right(curDiff), _) =>
          val complexityLimit =
            if (remainingLimit < Int.MaxValue) remainingLimit - curDiff.scriptsComplexity.toInt
            else remainingLimit

          val blockchain   = CompositeBlockchain(sblockchain, curDiff)
          val actionSender = Recipient.Address(ByteStr(tx.dAppAddressOrAlias.bytes))

          def applyTransfer(transfer: AssetTransfer, pk: PublicKey): TracedResult[FailedTransactionError, Diff] = {
            val AssetTransfer(addressRepr, amount, asset) = transfer
            val address                                   = Address.fromBytes(addressRepr.bytes.arr).explicitGet()
            Asset.fromCompatId(asset) match {
              case Waves =>
                val r = Diff.stateOps(
                  portfolios =
                    Map(address       -> Portfolio(amount, LeaseBalance.empty, Map.empty)) |+|
                      Map(dAppAddress -> Portfolio(-amount, LeaseBalance.empty, Map.empty))
                )
                TracedResult.wrapValue(r)
              case a @ IssuedAsset(id) =>
                val nextDiff = Diff.stateOps(
                  portfolios =
                    Map(address       -> Portfolio(0, LeaseBalance.empty, Map(a -> amount))) |+|
                      Map(dAppAddress -> Portfolio(0, LeaseBalance.empty, Map(a -> -amount)))
                )
                blockchain.assetScript(a).fold(TracedResult(nextDiff.asRight[FailedTransactionError])) {
                  case AssetScriptInfo(script, complexity) =>
                    val assetVerifierDiff =
                      if (blockchain.disallowSelfPayment) nextDiff
                      else
                        nextDiff.copy(
                          portfolios = Map(
                            address     -> Portfolio(0, LeaseBalance.empty, Map(a -> amount)),
                            dAppAddress -> Portfolio(0, LeaseBalance.empty, Map(a -> -amount))
                          )
                        )
                    val pseudoTx = ScriptTransfer(
                      asset,
                      actionSender,
                      pk,
                      Recipient.Address(addressRepr.bytes),
                      amount,
                      tx.timestamp,
                      tx.id()
                    )
                    val assetValidationDiff =
                      validatePseudoTxWithSmartAssetScript(blockchain, tx)(pseudoTx, a.id, assetVerifierDiff, script, complexity, complexityLimit)
                    val errorOpt = assetValidationDiff.fold(Some(_), _ => None)
                    TracedResult(
                      assetValidationDiff.map(d => nextDiff.copy(scriptsComplexity = d.scriptsComplexity)),
                      List(AssetVerifierTrace(id, errorOpt, AssetContext.Transfer))
                    )
                }
            }
          }

          def applyDataItem(item: DataOp): TracedResult[FailedTransactionError, Diff] =
            TracedResult.wrapValue(
              Diff.stateOps(accountData = Map(dAppAddress -> AccountDataInfo(Map(item.key -> dataItemToEntry(item)))))
            )

          def applyIssue(itx: InvokeScriptTransaction, pk: PublicKey, issue: Issue): TracedResult[FailedTransactionError, Diff] = {
            if (issue.name
                  .getBytes("UTF-8")
                  .length < IssueTransaction.MinAssetNameLength || issue.name.getBytes("UTF-8").length > IssueTransaction.MaxAssetNameLength) {
              TracedResult(Left(FailedTransactionError.dAppExecution("Invalid asset name", 0L)), List())
            } else if (issue.description.length > IssueTransaction.MaxAssetDescriptionLength) {
              TracedResult(Left(FailedTransactionError.dAppExecution("Invalid asset description", 0L)), List())
            } else if (blockchain.assetDescription(IssuedAsset(issue.id)).isDefined) {
              TracedResult(Left(FailedTransactionError.dAppExecution(s"Asset ${issue.id} is already issued", 0L)), List())
            } else {
              val staticInfo = AssetStaticInfo(TransactionId @@ itx.id(), pk, issue.decimals, blockchain.isNFT(issue))
              val volumeInfo = AssetVolumeInfo(issue.isReissuable, BigInt(issue.quantity))
              val info       = AssetInfo(ByteString.copyFromUtf8(issue.name), ByteString.copyFromUtf8(issue.description), Height @@ blockchain.height)

              val asset = IssuedAsset(issue.id)

              DiffsCommon
                .countVerifierComplexity(None /*issue.compiledScript*/, blockchain, isAsset = true)
                .map(
                  script =>
                    Diff(
                      tx = itx,
                      portfolios = Map(pk.toAddress -> Portfolio(assets = Map(asset -> issue.quantity))),
                      issuedAssets = Map(asset      -> NewAssetInfo(staticInfo, info, volumeInfo)),
                      assetScripts = Map(asset      -> script.map(script => AssetScriptInfo(script._1, script._2)))
                    )
                )
                .leftMap(asFailedScriptError)
            }
          }

          def applyReissue(reissue: Reissue, pk: PublicKey): TracedResult[FailedTransactionError, Diff] = {
            val reissueDiff = DiffsCommon.processReissue(blockchain, dAppAddress, blockTime, fee = 0, reissue).leftMap(asFailedScriptError)
            val pseudoTx    = ReissuePseudoTx(reissue, actionSender, pk, tx.id(), tx.timestamp)
            callAssetVerifierWithPseudoTx(reissueDiff, reissue.assetId, pseudoTx, AssetContext.Reissue)
          }

          def applyBurn(burn: Burn, pk: PublicKey): TracedResult[FailedTransactionError, Diff] = {
            val burnDiff = DiffsCommon.processBurn(blockchain, dAppAddress, fee = 0, burn).leftMap(asFailedScriptError)
            val pseudoTx = BurnPseudoTx(burn, actionSender, pk, tx.id(), tx.timestamp)
            callAssetVerifierWithPseudoTx(burnDiff, burn.assetId, pseudoTx, AssetContext.Burn)
          }

          def applySponsorFee(sponsorFee: SponsorFee, pk: PublicKey): TracedResult[FailedTransactionError, Diff] =
            for {
              _ <- TracedResult(
                Either.cond(
                  blockchain.assetDescription(IssuedAsset(sponsorFee.assetId)).exists(_.issuer == pk),
                  (),
                  FailedTransactionError.dAppExecution(s"SponsorFee assetId=${sponsorFee.assetId} was not issued from address of current dApp", 0L)
                )
              )
              _ <- TracedResult(SponsorFeeTxValidator.checkMinSponsoredAssetFee(sponsorFee.minSponsoredAssetFee).leftMap(asFailedScriptError))
              sponsorDiff = DiffsCommon.processSponsor(blockchain, dAppAddress, fee = 0, sponsorFee).leftMap(asFailedScriptError)
              pseudoTx    = SponsorFeePseudoTx(sponsorFee, actionSender, pk, tx.id(), tx.timestamp)
              r <- callAssetVerifierWithPseudoTx(sponsorDiff, sponsorFee.assetId, pseudoTx, AssetContext.Sponsor)
            } yield r

          def callAssetVerifierWithPseudoTx(
              actionDiff: Either[FailedTransactionError, Diff],
              assetId: ByteStr,
              pseudoTx: PseudoTx,
              assetType: AssetContext
          ): TracedResult[FailedTransactionError, Diff] =
            blockchain.assetScript(IssuedAsset(assetId)).fold(TracedResult(actionDiff)) {
              case AssetScriptInfo(script, complexity) =>
                val assetValidationDiff =
                  for {
                    result <- actionDiff
                    validatedResult <- validatePseudoTxWithSmartAssetScript(blockchain, tx)(
                      pseudoTx,
                      assetId,
                      result,
                      script,
                      complexity,
                      complexityLimit
                    )
                  } yield validatedResult
                val errorOpt = assetValidationDiff.fold(Some(_), _ => None)
                TracedResult(
                  assetValidationDiff,
                  List(AssetVerifierTrace(assetId, errorOpt, assetType))
                )
            }

          val diff = action match {
            case t: AssetTransfer =>
              applyTransfer(t, if (blockchain.isFeatureActivated(BlockV5)) {
                pk
              } else {
                PublicKey(new Array[Byte](32))
              })
            case d: DataOp      => applyDataItem(d)
            case i: Issue       => applyIssue(tx, pk, i)
            case r: Reissue     => applyReissue(r, pk)
            case b: Burn        => applyBurn(b, pk)
            case sf: SponsorFee => applySponsorFee(sf, pk)
          }
          diffAcc |+| diff.leftMap(_.addComplexity(curDiff.scriptsComplexity))

        case _ => diffAcc
      }
    }

  private def validatePseudoTxWithSmartAssetScript(blockchain: Blockchain, tx: InvokeScriptTransaction)(
      pseudoTx: PseudoTx,
      assetId: ByteStr,
      nextDiff: Diff,
      script: Script,
      complexity: Long,
      complexityLimit: Int
  ): Either[FailedTransactionError, Diff] =
    Try {
      ScriptRunner(
        Coproduct[TxOrd](pseudoTx),
        blockchain,
        script,
        isAssetScript = true,
        scriptContainerAddress =
          if (blockchain.passCorrectAssetId) Coproduct[Environment.Tthis](Environment.AssetId(assetId.arr))
          else Coproduct[Environment.Tthis](Environment.AssetId(tx.dAppAddressOrAlias.bytes)),
        complexityLimit
      ) match {
        case (log, Left(error))  => Left(FailedTransactionError.assetExecutionInAction(error, complexity, log, assetId))
        case (log, Right(FALSE)) => Left(FailedTransactionError.notAllowedByAssetInAction(complexity, log, assetId))
        case (_, Right(TRUE))    => Right(nextDiff.copy(scriptsComplexity = nextDiff.scriptsComplexity + complexity))
        case (log, Right(x)) =>
          Left(FailedTransactionError.assetExecutionInAction(s"Script returned not a boolean result, but $x", complexity, log, assetId))
      }
    } match {
      case Failure(e) =>
        Left(
          FailedTransactionError
            .assetExecutionInAction(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", complexity, List.empty, assetId)
        )
      case Success(s) => s
    }

  private def asFailedScriptError(ve: ValidationError): FailedTransactionError =
    ve match {
      case e: FailedTransactionError => e
      case e: GenericError           => FailedTransactionError.dAppExecution(e.err, 0L)
      case e                         => FailedTransactionError.dAppExecution(e.toString, 0L)
    }
}

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
import com.wavesplatform.transaction.ApplicationStatus.{ScriptExecutionFailed, Succeeded}
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
  def calcAndCheckFee[E <: ValidationError](
      makeError: (String, Long) => E,
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      stepLimit: Long,
      invocationComplexity: Long,
      issueList: List[Issue],
      actionScriptsInvoked: Int
  ): TracedResult[ValidationError, Map[Address, Portfolio]] =
    TracedResult {
      val stepsNumber =
        if (invocationComplexity % stepLimit == 0)
          invocationComplexity / stepLimit
        else
          invocationComplexity / stepLimit + 1

      val attachedFee = tx.fee

      for {
        (attachedFeeInWaves, portfolioDiff) <- tx.assetFee._1 match {
          case Waves => Right((attachedFee, Map(tx.sender.toAddress -> Portfolio(-attachedFee))))
          case asset @ IssuedAsset(_) =>
            for {
              assetInfo <- blockchain
                .assetDescription(asset)
                .toRight(GenericError(s"Asset $asset does not exist, cannot be used to pay fees"))
              feeInWaves <- Either.cond(
                assetInfo.sponsorship > 0,
                Sponsorship.toWaves(attachedFee, assetInfo.sponsorship),
                GenericError(s"Asset $asset is not sponsored, cannot be used to pay fees")
              )
            } yield {
              val portfolioDiff =
                Map(tx.sender.toAddress          -> Portfolio(assets = Map(asset              -> -attachedFee))) |+|
                  Map(assetInfo.issuer.toAddress -> Portfolio(-feeInWaves, assets = Map(asset -> attachedFee)))
              (feeInWaves, portfolioDiff)
            }
        }
        _ <- {
          val dAppFee    = expectedStepFeeInWaves(tx, blockchain) * stepsNumber
          val issuesFee  = issueList.count(!blockchain.isNFT(_)) * FeeConstants(IssueTransaction.typeId) * FeeUnit
          val actionsFee = actionScriptsInvoked * ScriptExtraFee
          val minFee     = dAppFee + issuesFee + actionsFee

          lazy val errorMessage = {
            val stepsInfo =
              if (stepsNumber > 1)
                s" with $stepsNumber invocation steps"
              else
                ""

            val extraFeePerStepInfo =
              if (tx.extraFeePerStep > InvokeScriptTransaction.DefaultExtraFeePerStep)
                s" with extra fee per step = ${tx.extraFeePerStep}"
              else
                ""

            val totalScriptsInvokedInfo =
              if (actionScriptsInvoked > 0)
                s" with $actionScriptsInvoked total scripts invoked"
              else
                ""

            val issuesInfo =
              if (issueList.nonEmpty)
                s" with ${issueList.length} assets issued"
              else
                ""

            val assetName = tx.assetFee._1.fold("WAVES")(_.id.toString)
            val txName    = Constants.TransactionNames(InvokeScriptTransaction.typeId)

            s"Fee in $assetName for $txName (${tx.assetFee._2} in $assetName)" +
              s"$stepsInfo$extraFeePerStepInfo$totalScriptsInvokedInfo$issuesInfo " +
              s"does not exceed minimal value of $minFee WAVES."
          }

          Either.cond(
            attachedFeeInWaves >= minFee,
            (),
            makeError(errorMessage, invocationComplexity)
          )
        }
      } yield portfolioDiff
    }

  private def expectedStepFeeInAttachedAsset(tx: InvokeScriptTransaction, blockchain: Blockchain): Long =
    wavesToAttachedAsset(tx, blockchain, FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit) + tx.extraFeePerStep

  private def expectedStepFeeInWaves(tx: InvokeScriptTransaction, blockchain: Blockchain): Long =
    FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit + attachedAssetToWaves(tx, blockchain, tx.extraFeePerStep)

  private def wavesToAttachedAsset(
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      waves: Long
  ): Long =
    invokeSponsorshipConverter(tx, blockchain, Sponsorship.fromWaves)(waves)

  private def attachedAssetToWaves(
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      attachedAsset: Long
  ): Long =
    invokeSponsorshipConverter(tx, blockchain, Sponsorship.toWaves)(attachedAsset)

  private def invokeSponsorshipConverter(
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      converter: (Long, Long) => Long
  ): Long => Long =
    amount =>
      tx.assetFee._1 match {
        case Waves =>
          amount
        case asset @ IssuedAsset(_) =>
          val assetInfo = blockchain.assetDescription(asset).get
          converter(amount, assetInfo.sponsorship)
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
      invocationComplexity: Long,
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      blockTime: Long,
      isContinuation: Boolean, // TODO refactor?
      limitedExecution: Boolean
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

      actionAssets = tx.checkedAssets ++
        transferList.flatMap(_.assetId).map(IssuedAsset) ++
        reissueList.map(r => IssuedAsset(r.assetId)) ++
        burnList.map(b => IssuedAsset(b.assetId)) ++
        sponsorFeeList.map(sf => IssuedAsset(sf.assetId))

      actionScriptsInvoked = actionAssets.count(blockchain.hasAssetScript) +
        (if (blockchain.hasAccountScript(tx.sender.toAddress)) 1 else 0)

      stepLimit = ContractLimits.MaxComplexityByVersion(version)
      feeDiff <- if (isContinuation)
        TracedResult.wrapValue(Map[Address, Portfolio]())
      else
        calcAndCheckFee(
          FailedTransactionError.feeForActions,
          tx,
          blockchain,
          stepLimit,
          invocationComplexity,
          issueList,
          actionScriptsInvoked
        )

      paymentsAndFeeDiff = if (isContinuation) Diff(tx = tx) else paymentsPart(tx, dAppAddress, feeDiff)

      compositeDiff <- foldActions(blockchain, blockTime, tx, dAppAddress, dAppPublicKey)(actions, paymentsAndFeeDiff, complexityLimit)
        .leftMap(_.addComplexity(invocationComplexity))

      transfers = compositeDiff.portfolios |+| feeDiff.view.mapValues(_.negate).toMap

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
        scriptsRun = if (isContinuation) 0 else actionScriptsInvoked + 1,
        scriptResults = Map(tx.id() -> isr),
        scriptsComplexity = (if (isContinuation) 0 else invocationComplexity) + compositeDiff.scriptsComplexity
      )
    } yield resultDiff
  }

  def finishContinuation(
      diff: Diff,
      tx: ContinuationTransaction,
      blockchain: Blockchain,
      invoke: InvokeScriptTransaction,
      spentComplexity: Long,
      failed: Boolean
  ): Diff = {
    val dAppAddress                      = blockchain.resolveAlias(invoke.dAppAddressOrAlias).explicitGet()
    val diffWithState                    = diff.addContinuationState(dAppAddress, ContinuationState.Finished)
    val StepInfo(_, stepFee, scriptsRun) = stepInfo(diffWithState, blockchain, invoke)
    val status                           = if (failed) ScriptExecutionFailed else Succeeded
    diffWithState |+| Diff.empty.copy(
      portfolios = refundablePortfolios(dAppAddress, invoke, blockchain, stepFee, failed),
      scriptsRun = scriptsRun,
      scriptsComplexity = spentComplexity,
      replacingTransactions = Seq(
        NewTransactionInfo(tx.copy(fee = stepFee), Set(), status),
        NewTransactionInfo(invoke, (diffWithState.portfolios.keys ++ diffWithState.accountData.keys).toSet, status)
      )
    )
  }

  def stepInfo(
      diff: Diff,
      blockchain: Blockchain,
      invoke: InvokeScriptTransaction
  ): StepInfo = {
    val dAppAddress = blockchain.resolveAlias(invoke.dAppAddressOrAlias).explicitGet()
    val isFirstStep = diff.continuationStates.get(dAppAddress).exists {
      case s: ContinuationState.InProgress if s.precedingStepCount == 0 => true
      case _                                                            => false
    }
    val isLastStep = diff.continuationStates.get(dAppAddress).contains(ContinuationState.Finished)

    val scriptResult = diff.scriptResults.getOrElse(invoke.id.value(), InvokeScriptResult())
    val assetActions =
      if (isLastStep)
        scriptResult.transfers.flatMap(_.asset.fold(Option.empty[IssuedAsset])(Some(_))) ++
          scriptResult.burns.map(b => IssuedAsset(b.assetId)) ++
          scriptResult.reissues.map(r => IssuedAsset(r.assetId)) ++
          scriptResult.sponsorFees.map(sf => IssuedAsset(sf.assetId))
      else
        Seq()
    val smartAccountCount    = if (isFirstStep && blockchain.hasAccountScript(invoke.sender.toAddress)) 1 else 0
    val paymentsCount        = if (isFirstStep) invoke.checkedAssets.count(blockchain.hasAssetScript) else 0
    val additionalScriptsRun = smartAccountCount + paymentsCount + assetActions.count(blockchain.hasAssetScript)
    val additionalScriptsFee = additionalScriptsRun * ScriptExtraFee
    val issuesFee            = scriptResult.issues.count(!blockchain.isNFT(_)) * FeeConstants(IssueTransaction.typeId) * FeeUnit
    val baseStepFee          = expectedStepFeeInWaves(invoke, blockchain)
    val feeInWaves           = baseStepFee + additionalScriptsFee + issuesFee
    val feeInAttachedAsset   = wavesToAttachedAsset(invoke, blockchain, feeInWaves)
    StepInfo(feeInWaves, feeInAttachedAsset, scriptsRun = 1 + additionalScriptsRun)
  }

  private def refundablePortfolios(
      dAppAddress: Address,
      invoke: InvokeScriptTransaction,
      blockchain: Blockchain,
      lastStepFee: Long,
      failed: Boolean
  ): Map[Address, Portfolio] = {
    val precedingStepCount = blockchain.continuationStates(dAppAddress).asInstanceOf[ContinuationState.InProgress].precedingStepCount
    val consumedFee        = (precedingStepCount + 1) * expectedStepFeeInAttachedAsset(invoke, blockchain) + lastStepFee
    val unusedFee          = invoke.fee - consumedFee
    val invoker            = invoke.sender.toAddress
    val unusedFeePortfolio = invoke.assetFee._1 match {
      case Waves =>
        Map(invoker -> Portfolio(unusedFee))
      case asset @ IssuedAsset(_) =>
        val assetInfo  = blockchain.assetDescription(asset).get
        val issuer     = assetInfo.issuer.toAddress
        val feeInWaves = Sponsorship.toWaves(unusedFee, assetInfo.sponsorship)
        Map(invoker  -> Portfolio.build(asset, unusedFee)) |+|
          Map(issuer -> Portfolio(feeInWaves, assets = Map(asset -> -unusedFee)))
    }
    val paymentPortfolio =
      if (failed)
        invoke.payments.map(p => Portfolio.build(p.assetId, p.amount)).fold(Portfolio.empty)(_ |+| _)
      else
        Portfolio.empty
    unusedFeePortfolio |+| Map(invoker -> paymentPortfolio, dAppAddress -> paymentPortfolio.negate)
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
              Map(tx.sender.toAddress -> Portfolio(assets = Map(asset -> -amt))) |+|
                Map(dAppAddress    -> Portfolio(assets = Map(asset -> amt)))
            case Waves =>
              Map(tx.sender.toAddress -> Portfolio(-amt)) |+|
                Map(dAppAddress    -> Portfolio(amt))
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

          val blockchain   = CompositeBlockchain(sblockchain, Some(curDiff))
          val actionSender = Recipient.Address(ByteStr(tx.dAppAddressOrAlias.bytes))

          def applyTransfer(transfer: AssetTransfer, pk: PublicKey): TracedResult[FailedTransactionError, Diff] = {
            val AssetTransfer(addressRepr, amount, asset) = transfer
            val address                                   = Address.fromBytes(addressRepr.bytes.arr).explicitGet()
            Asset.fromCompatId(asset) match {
              case Waves =>
                val r = Diff.stateOps(
                  portfolios =
                    Map(address       -> Portfolio(amount)) |+|
                      Map(dAppAddress -> Portfolio(-amount))
                )
                TracedResult.wrapValue(r)
              case a @ IssuedAsset(id) =>
                val nextDiff = Diff.stateOps(
                  portfolios =
                    Map(address       -> Portfolio(assets = Map(a -> amount))) |+|
                      Map(dAppAddress -> Portfolio(assets = Map(a -> -amount)))
                )
                blockchain.assetScript(a).fold(TracedResult(nextDiff.asRight[FailedTransactionError])) {
                  case AssetScriptInfo(script, complexity) =>
                    val assetVerifierDiff =
                      if (blockchain.disallowSelfPayment) nextDiff
                      else
                        nextDiff.copy(
                          portfolios = Map(
                            address     -> Portfolio(assets = Map(a -> amount)),
                            dAppAddress -> Portfolio(assets = Map(a -> -amount))
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
                      validatePseudoTxWithSmartAssetScript(blockchain, tx)(
                        pseudoTx,
                        a.id,
                        assetVerifierDiff,
                        script,
                        complexity,
                        complexityLimit
                      )
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

  case class StepInfo(
      feeInWaves: Long,
      feeInAttachedAsset: Long,
      scriptsRun: Int
  )
}

package com.wavesplatform.state.diffs.invoke

import cats.instances.map._
import cats.syntax.either._
import cats.syntax.semigroup._
import com.google.common.base.Throwables
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.features.FeatureProvider.FeatureProviderExt
import com.wavesplatform.features.InvokeScriptSelfPaymentPolicyProvider._
import com.wavesplatform.features.ScriptTransferValidationProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.traits.domain.Tx.{BurnPseudoTx, ReissuePseudoTx, ScriptTransfer}
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
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, TracedResult}
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
    val complexityOpt =
      for {
        complexitiesByCallable <- callableComplexities.get(blockchain.estimator.version)
        complexity             <- complexitiesByCallable.get(tx.funcCall.function.funcName)
      } yield complexity

    lazy val errorMessage =
      s"Cannot find callable function `${tx.funcCall.function.funcName}` complexity, " +
        s"address = $dAppAddress, " +
        s"estimator version = ${blockchain.estimator.version}"

    complexityOpt.toRight(GenericError(errorMessage))
  }

  def processActions(
      actions: List[CallableAction],
      version: StdLibVersion,
      dAppAddress: Address,
      dAppPublicKey: PublicKey,
      feeInfo: (Long, Map[Address, Portfolio]),
      invocationComplexity: Long,
      verifierComplexity: Long,
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      blockTime: Long
  ): TracedResult[ValidationError, Diff] = {
    val actionsByType = actions.groupBy(_.getClass).withDefaultValue(Nil)
    val transferList  = actionsByType(classOf[AssetTransfer]).asInstanceOf[List[AssetTransfer]]
    val issueList     = actionsByType(classOf[Issue]).asInstanceOf[List[Issue]]
    val reissueList   = actionsByType(classOf[Reissue]).asInstanceOf[List[Reissue]]
    val burnList      = actionsByType(classOf[Burn]).asInstanceOf[List[Burn]]

    val dataEntries = actionsByType
      .filterKeys(classOf[DataItem[_]].isAssignableFrom)
      .values
      .flatten
      .toList
      .asInstanceOf[List[DataItem[_]]]
      .map(dataItemToEntry)

    for {
      _ <- TracedResult(checkDataEntries(tx, dataEntries)).leftMap(ScriptExecutionError.dApp)
      _ <- TracedResult(
        Either.cond(
          actions.length - dataEntries.length <= ContractLimits.MaxCallableActionsAmount,
          (),
          ScriptExecutionError.dApp(s"Too many script actions: max: ${ContractLimits.MaxCallableActionsAmount}, actual: ${actions.length}")
        )
      )

      _ <- TracedResult(checkSelfPayments(dAppAddress, blockchain, tx, version, transferList))
      _ <- TracedResult(Either.cond(transferList.map(_.amount).forall(_ >= 0), (), ScriptExecutionError.dApp("Negative amount")))
      _ <- TracedResult(checkOverflow(transferList.map(_.amount)))

      assetsComplexity = (tx.checkedAssets.map(_.id) ++ transferList.flatMap(_.assetId))
        .flatMap(id => blockchain.assetScript(IssuedAsset(id)))
        .map(_._2)

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
            burnList.map(b => IssuedAsset(b.assetId))
        val totalScriptsInvoked = smartAssetInvocations.count(blockchain.hasAssetScript) + (if (blockchain.hasAccountScript(tx.sender)) 1 else 0)
        val minIssueFee         = issueList.count(i => !blockchain.isNFT(i)) * FeeConstants(IssueTransaction.typeId) * FeeUnit
        val dAppInvocationFee   = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit * stepsNumber
        val minWaves            = totalScriptsInvoked * ScriptExtraFee + dAppInvocationFee + minIssueFee
        val txName              = Constants.TransactionNames(InvokeScriptTransaction.typeId)
        val assetName           = tx.assetFee._1.fold("WAVES")(_.id.toString)
        Either.cond(
          minWaves <= feeInfo._1,
          totalScriptsInvoked,
          InsufficientInvokeActionFee(
            s"Fee in $assetName for $txName (${tx.assetFee._2} in $assetName)" +
              s" with $totalScriptsInvoked total scripts invoked$stepsInfo does not exceed minimal value of $minWaves WAVES."
          )
        )
      }

      paymentsAndFeeDiff = paymentsPart(tx, dAppAddress, feeInfo._2)

      compositeDiff <- foldActions(blockchain, blockTime, tx, dAppAddress, dAppPublicKey)(actions, paymentsAndFeeDiff).leftMap(asFailedScriptError)

      transfers = compositeDiff.portfolios |+| feeInfo._2.mapValues(_.negate)

      currentTxDiff         = compositeDiff.transactions(tx.id())
      currentTxDiffWithKeys = currentTxDiff.copy(_2 = currentTxDiff._2 ++ transfers.keys ++ compositeDiff.accountData.keys)
      updatedTxDiff         = compositeDiff.transactions.updated(tx.id(), currentTxDiffWithKeys)

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
        burnList
      )

      resultDiff = compositeDiff.copy(
        transactions = updatedTxDiff,
        scriptsRun = scriptsInvoked + 1,
        scriptResults = Map(tx.id() -> isr),
        scriptsComplexity = invocationComplexity + verifierComplexity + assetsComplexity.sum
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

  private def dataItemToEntry(item: DataItem[_]): DataEntry[_] =
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
  ): Either[ScriptExecutionError, Unit] =
    if (blockchain.disallowSelfPayment && version >= V4)
      if (tx.payments.nonEmpty && tx.sender.toAddress == dAppAddress)
        ScriptExecutionError.dApp("DApp self-payment is forbidden since V4").asLeft[Unit]
      else if (transfers.exists(_.recipient.bytes == dAppAddress.bytes))
        ScriptExecutionError.dApp("DApp self-transfer is forbidden since V4").asLeft[Unit]
      else
        ().asRight[ScriptExecutionError]
    else
      ().asRight[ScriptExecutionError]

  private def checkOverflow(dataList: Traversable[Long]): Either[ScriptExecutionError, Unit] = {
    Try(dataList.foldLeft(0L)(Math.addExact))
      .fold(
        _ => ScriptExecutionError.dApp("Attempt to transfer unavailable funds in contract payment").asLeft[Unit],
        _ => ().asRight[ScriptExecutionError]
      )
  }

  private[this] def checkDataEntries(
      tx: InvokeScriptTransaction,
      dataEntries: Seq[DataEntry[_]]
  ): Either[String, Unit] =
    for {
      _ <- Either.cond(
        dataEntries.length <= ContractLimits.MaxWriteSetSize,
        (),
        s"WriteSet can't contain more than ${ContractLimits.MaxWriteSetSize} entries"
      )
      _ <- Either.cond(
        dataEntries.forall(_.key.utf8Bytes.length <= ContractLimits.MaxKeySizeInBytes),
        (),
        s"Key size must be less than ${ContractLimits.MaxKeySizeInBytes}"
      )

      totalDataBytes = dataEntries.map(_.toBytes.length).sum
      _ <- Either.cond(
        totalDataBytes <= ContractLimits.MaxWriteSetSizeInBytes,
        (),
        s"WriteSet size can't exceed ${ContractLimits.MaxWriteSetSizeInBytes} bytes, actual: $totalDataBytes bytes"
      )
      _ <- Either.cond(
        !tx.isProtobufVersion || dataEntries.forall(_.key.nonEmpty),
        (),
        s"Empty keys aren't allowed in tx version >= ${tx.protobufVersion}"
      )
    } yield ()

  private def foldActions(sblockchain: Blockchain, blockTime: Long, tx: InvokeScriptTransaction, dAppAddress: Address, pk: PublicKey)(
      ps: List[CallableAction],
      paymentsDiff: Diff
  ): TracedResult[ValidationError, Diff] =
    ps.foldLeft(TracedResult(paymentsDiff.asRight[ValidationError])) { (diffAcc, action) =>
      diffAcc match {
        case TracedResult(Right(curDiff), _) =>
          val blockchain   = CompositeBlockchain(sblockchain, Some(curDiff))
          val actionSender = Recipient.Address(tx.dAppAddressOrAlias.bytes)

          def applyTransfer(transfer: AssetTransfer, pk: PublicKey): TracedResult[ValidationError, Diff] = {
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
                blockchain.assetScript(a) match {
                  case None => nextDiff.asRight[ValidationError]
                  case Some((script, _)) =>
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
                    val assetValidationDiff = validatePseudoTxWithSmartAssetScript(blockchain, tx)(pseudoTx, a.id, assetVerifierDiff, script)
                    val errorOpt            = assetValidationDiff.fold(Some(_), _ => None)
                    TracedResult(
                      assetValidationDiff.map(_ => nextDiff),
                      List(AssetVerifierTrace(id, errorOpt))
                    )
                }
            }
          }

          def applyDataItem(item: DataItem[_]): TracedResult[ValidationError, Diff] =
            TracedResult.wrapValue(
              Diff.stateOps(accountData = Map(dAppAddress -> AccountDataInfo(Map(item.key -> dataItemToEntry(item)))))
            )

          def applyIssue(itx: InvokeScriptTransaction, pk: PublicKey, issue: Issue): TracedResult[ValidationError, Diff] = {
            if (issue.name
                  .getBytes("UTF-8")
                  .length < IssueTransaction.MinAssetNameLength || issue.name.getBytes("UTF-8").length > IssueTransaction.MaxAssetNameLength) {
              TracedResult(Left(InvalidName), List())
            } else if (issue.description.length > IssueTransaction.MaxAssetDescriptionLength) {
              TracedResult(Left(TooBigArray), List())
            } else if (blockchain.assetDescription(IssuedAsset(issue.id)).isDefined) {
              TracedResult(Left(ScriptExecutionError(s"Asset ${issue.id} is already issued", List(), isAssetScript = false)), List())
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
                      portfolios = Map(pk.toAddress -> Portfolio(balance = 0, lease = LeaseBalance.empty, assets = Map(asset -> issue.quantity))),
                      issuedAssets = Map(asset      -> ((staticInfo, info, volumeInfo))),
                      assetScripts = Map(asset      -> script.map(script => (script._1, script._2)))
                    )
                )
            }
          }

          def applyReissue(reissue: Reissue, pk: PublicKey): TracedResult[ValidationError, Diff] = {
            val reissueDiff = DiffsCommon.processReissue(blockchain, dAppAddress, blockTime, fee = 0, reissue)
            val pseudoTx    = ReissuePseudoTx(reissue, actionSender, pk, tx.id(), tx.timestamp)
            validateActionAsPseudoTx(reissueDiff, reissue.assetId, pseudoTx)
          }

          def applyBurn(burn: Burn, pk: PublicKey): TracedResult[ValidationError, Diff] = {
            val burnDiff = DiffsCommon.processBurn(blockchain, dAppAddress, fee = 0, burn)
            val pseudoTx = BurnPseudoTx(burn, actionSender, pk, tx.id(), tx.timestamp)
            validateActionAsPseudoTx(burnDiff, burn.assetId, pseudoTx)
          }

          def validateActionAsPseudoTx(
              actionDiff: Either[ValidationError, Diff],
              assetId: ByteStr,
              pseudoTx: PseudoTx
          ): TracedResult[ValidationError, Diff] =
            blockchain.assetScript(IssuedAsset(assetId)) match {
              case None => actionDiff
              case Some((script, _)) =>
                val assetValidationDiff =
                  for {
                    result          <- actionDiff
                    validatedResult <- validatePseudoTxWithSmartAssetScript(blockchain, tx)(pseudoTx, assetId, result, script)
                  } yield validatedResult
                val errorOpt = assetValidationDiff.fold(Some(_), _ => None)
                TracedResult(
                  assetValidationDiff,
                  List(AssetVerifierTrace(assetId, errorOpt))
                )
            }

          val diff = action match {
            case t: AssetTransfer =>
              applyTransfer(t, if (blockchain.isFeatureActivated(BlockV5)) {
                pk
              } else {
                PublicKey(ByteStr.empty)
              })
            case d: DataItem[_] => applyDataItem(d)
            case i: Issue       => applyIssue(tx, pk, i)
            case r: Reissue     => applyReissue(r, pk)
            case b: Burn        => applyBurn(b, pk)
          }
          diffAcc |+| diff

        case _ => diffAcc
      }
    }

  private def validatePseudoTxWithSmartAssetScript(blockchain: Blockchain, tx: InvokeScriptTransaction)(
      pseudoTx: PseudoTx,
      assetId: ByteStr,
      nextDiff: Diff,
      script: Script
  ): Either[ValidationError, Diff] =
    Try {
      ScriptRunner(
        Coproduct[TxOrd](pseudoTx),
        blockchain,
        script,
        isAssetScript = true,
        scriptContainerAddress = if (blockchain.passCorrectAssetId) assetId else tx.dAppAddressOrAlias.bytes
      ) match {
        case (log, Left(error))  => Left(ScriptExecutionError.asset(error, log, FailedScriptError.Reason.AssetInAction))
        case (log, Right(FALSE)) => Left(TransactionNotAllowedByScript.asset(log, FailedScriptError.Reason.AssetInAction))
        case (_, Right(TRUE))    => Right(nextDiff)
        case (log, Right(x)) =>
          Left(ScriptExecutionError.asset(s"Script returned not a boolean result, but $x", log, FailedScriptError.Reason.AssetInAction))
      }
    } match {
      case Failure(e) =>
        Left(
          ScriptExecutionError.asset(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, FailedScriptError.Reason.AssetInAction)
        )
      case Success(s) => s
    }

  private def asFailedScriptError(ve: ValidationError): FailedScriptError =
    ve match {
      case e: FailedScriptError => e
      case e: GenericError      => ScriptExecutionError.dApp(e.err)
      case e                    => ScriptExecutionError.dApp(e.toString)
    }
}

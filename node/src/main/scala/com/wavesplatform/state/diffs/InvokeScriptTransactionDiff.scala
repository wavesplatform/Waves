package com.wavesplatform.state.diffs

import cats.instances.map._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.semigroup._
import com.google.common.base.Throwables
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.features.FeatureProvider.FeatureProviderExt
import com.wavesplatform.features.FunctionCallPolicyProvider._
import com.wavesplatform.features.InvokeScriptSelfPaymentPolicyProvider._
import com.wavesplatform.features.ScriptTransferValidationProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.ContractCompiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Tx.{BurnPseudoTx, ReissuePseudoTx, ScriptTransfer}
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.metrics._
import com.wavesplatform.settings.Constants
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.CommonValidation._
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils._
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.{Failure, Right, Success, Try}

object InvokeScriptTransactionDiff {

  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(blockchain: Blockchain, blockTime: Long)(tx: InvokeScriptTransaction): TracedResult[ValidationError, Diff] = {

    val dAppAddressEi = blockchain.resolveAlias(tx.dAppAddressOrAlias)
    val accScriptEi   = dAppAddressEi.map(blockchain.accountScript)
    val functionCall  = tx.funcCall

    accScriptEi match {
      case Right(Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, storedCallableComplexities))) =>
        for {
          _           <- TracedResult.wrapE(checkCall(functionCall, blockchain).leftMap(GenericError.apply))
          dAppAddress <- TracedResult(dAppAddressEi)

          feeInfo <- TracedResult(tx.assetFee._1 match {
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
          })
          wavesFee     = feeInfo._1
          paymentsDiff = paymentsPart(blockchain.height, tx, dAppAddress, feeInfo._2)

          directives <- TracedResult.wrapE(DirectiveSet(version, Account, DAppType).leftMap(GenericError.apply))
          payments   <- TracedResult.wrapE(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DApp).leftMap(GenericError.apply))
          input      <- TracedResult.wrapE(buildThisValue(Coproduct[TxOrd](tx: Transaction), blockchain, directives, None).leftMap(GenericError.apply))

          invocationComplexity <- TracedResult.wrapE({
            val complexity =
              for {
                complexitiesByCallable <- storedCallableComplexities.get(blockchain.estimator.version)
                complexity             <- complexitiesByCallable.get(tx.funcCall.function.funcName)
              } yield complexity

            lazy val errorMessage =
              s"Cannot find callable function `${tx.funcCall.function.funcName}` complexity, " +
                s"address = $dAppAddress, " +
                s"estimator version = ${blockchain.estimator.version}"

            complexity.toRight(GenericError(errorMessage))

          })

          scriptResult <- {
            val scriptResultE = stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
              val invoker = tx.sender.toAddress.bytes
              val invocation = ContractEvaluator.Invocation(
                functionCall,
                Recipient.Address(invoker),
                tx.sender,
                payments,
                tx.dAppAddressOrAlias.bytes,
                tx.id.value,
                tx.fee,
                tx.feeAssetId.compatId
              )
              val environment = new WavesEnvironment(
                AddressScheme.current.chainId,
                Coeval.evalOnce(input),
                Coeval(blockchain.height),
                blockchain,
                Coeval(tx.dAppAddressOrAlias.bytes),
                directives,
                tx.id()
              )

              val result = for {
                evaluator <- ContractEvaluator(
                  Monoid
                    .combineAll(
                      Seq(
                        PureContext.build(Global, version).withEnvironment[Environment],
                        CryptoContext.build(Global, version).withEnvironment[Environment],
                        WavesContext.build(directives)
                      )
                    )
                    .evaluationContext(environment),
                  contract,
                  invocation,
                  version
                )
              } yield (evaluator, invocationComplexity)

              result.leftMap { case (error, log) => ScriptExecutionError.dApp(error, log) }
            })
            TracedResult(
              scriptResultE,
              List(InvokeScriptTrace(tx.dAppAddressOrAlias, functionCall, scriptResultE.map(_._1._1), scriptResultE.fold(_.log, _._1._2)))
            )
          }

          invocationComplexity = scriptResult._2
          actions = scriptResult._1._1 match {
            case ScriptResultV3(dataItems, transfers) => dataItems ::: transfers
            case ScriptResultV4(actions)              => actions
          }

          actionsByType = actions.groupBy(_.getClass).withDefaultValue(Nil)
          transferList  = actionsByType(classOf[AssetTransfer]).asInstanceOf[List[AssetTransfer]]
          issueList     = actionsByType(classOf[Issue]).asInstanceOf[List[Issue]]
          reissueList   = actionsByType(classOf[Reissue]).asInstanceOf[List[Reissue]]
          burnList      = actionsByType(classOf[Burn]).asInstanceOf[List[Burn]]

          dataItems = actionsByType
            .filterKeys(classOf[DataItem[_]].isAssignableFrom)
            .values
            .flatten
            .toList
            .asInstanceOf[List[DataItem[_]]]

          dataEntries = dataItems.map(dataItemToEntry)

          _ <- TracedResult(checkDataEntries(tx, dataEntries)).leftMap(GenericError(_))
          _ <- TracedResult(
            Either.cond(
              actions.length - dataEntries.length <= ContractLimits.MaxCallableActionsAmount,
              (),
              GenericError(s"Too many script actions: max: ${ContractLimits.MaxCallableActionsAmount}, actual: ${actions.length}")
            )
          )

          _ <- TracedResult(checkSelfPayments(dAppAddress, blockchain, tx, version, transferList))
          _ <- TracedResult(Either.cond(transferList.map(_.amount).forall(_ >= 0), (), NegativeAmount(-42, "")))
          _ <- TracedResult(validateOverflow(transferList.map(_.amount), "Attempt to transfer unavailable funds in contract payment"))

          verifierComplexity = blockchain.accountScript(tx.sender).map(_.verifierComplexity)
          assetsComplexity = (tx.checkedAssets.map(_.id) ++ transferList.flatMap(_.assetId))
            .flatMap(id => blockchain.assetScript(IssuedAsset(id)))
            .map(_._2)

          scriptsInvoked <- TracedResult {
            val smartAssetInvocations =
              tx.checkedAssets ++
                transferList.flatMap(_.assetId).map(IssuedAsset) ++
                reissueList.map(r => IssuedAsset(r.assetId)) ++
                burnList.map(b => IssuedAsset(b.assetId))
            val totalScriptsInvoked = smartAssetInvocations.count(blockchain.hasAssetScript) + (if (blockchain.hasAccountScript(tx.sender)) 1 else 0)
            val minIssueFee         = issueList.count(i => !blockchain.isNFT(i)) * FeeConstants(IssueTransaction.typeId) * FeeUnit
            val minWaves            = totalScriptsInvoked * ScriptExtraFee + FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit + minIssueFee
            val txName              = Constants.TransactionNames(InvokeScriptTransaction.typeId)
            val assetName           = tx.assetFee._1.fold("WAVES")(_.id.toString)
            Either.cond(
              minWaves <= wavesFee,
              totalScriptsInvoked,
              InsufficientInvokeActionFee(
                s"Fee in $assetName for $txName (${tx.assetFee._2} in $assetName)" +
                  s" with $totalScriptsInvoked total scripts invoked does not exceed minimal value of $minWaves WAVES."
              )
            )
          }
          compositeDiff <- foldActions(blockchain, blockTime, tx, dAppAddress, pk)(actions, paymentsDiff)
        } yield {
          val transfers = compositeDiff.portfolios |+| feeInfo._2.mapValues(_.negate)

          val currentTxDiff         = compositeDiff.transactions(tx.id())
          val currentTxDiffWithKeys = currentTxDiff.copy(_2 = currentTxDiff._2 ++ transfers.keys ++ compositeDiff.accountData.keys)
          val updatedTxDiff         = compositeDiff.transactions.updated(tx.id(), currentTxDiffWithKeys)

          val isr = InvokeScriptResult(
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

          compositeDiff.copy(
            transactions = updatedTxDiff,
            scriptsRun = scriptsInvoked + 1,
            scriptResults = Map(tx.id() -> isr),
            scriptsComplexity = invocationComplexity + verifierComplexity.getOrElse(0L) + assetsComplexity.sum
          )
        }
      case Left(l) => TracedResult(Left(l))
      case _       => TracedResult(Left(GenericError(s"No contract at address ${tx.dAppAddressOrAlias}")))
    }
  }

  private def checkCall(fc: FUNCTION_CALL, blockchain: Blockchain): Either[ExecutionError, Unit] = {
    val (check, expectedTypes) =
      if (blockchain.callableListArgumentsAllowed)
        (
          fc.args.forall(arg => arg.isInstanceOf[EVALUATED] && !arg.isInstanceOf[CaseObj]),
          ContractCompiler.allowedCallableTypesV4
        )
      else
        (
          fc.args.forall(arg => arg.isInstanceOf[EVALUATED] && !arg.isInstanceOf[CaseObj] && !arg.isInstanceOf[ARR]),
          ContractCompiler.primitiveCallableTypes
        )
    Either.cond(
      check,
      (),
      s"All arguments of InvokeScript must be one of the types: ${expectedTypes.mkString(", ")}"
    )
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
  ): Either[GenericError, Unit] =
    if (blockchain.disallowSelfPayment && version >= V4)
      if (tx.payments.nonEmpty && tx.sender.toAddress == dAppAddress)
        GenericError("DApp self-payment is forbidden since V4").asLeft[Unit]
      else if (transfers.exists(_.recipient.bytes == dAppAddress.bytes))
        GenericError("DApp self-transfer is forbidden since V4").asLeft[Unit]
      else
        ().asRight[GenericError]
    else
      ().asRight[GenericError]

  private def paymentsPart(
      height: Int,
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

  private[this] def checkDataEntries(tx: InvokeScriptTransaction, dataEntries: Seq[DataEntry[_]]): Either[String, Unit] =
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
        case (log, Left(error))  => Left(ScriptExecutionError.asset(error, log, CanFail.Reason.AssetInAction))
        case (log, Right(FALSE)) => Left(TransactionNotAllowedByScript.asset(log, CanFail.Reason.AssetInAction))
        case (_, Right(TRUE))    => Right(nextDiff)
        case (log, Right(x))     => Left(ScriptExecutionError.asset(s"Script returned not a boolean result, but $x", log, CanFail.Reason.AssetInAction))
      }
    } match {
      case Failure(e) =>
        Left(
          ScriptExecutionError.asset(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, CanFail.Reason.AssetInAction)
        )
      case Success(s) => s
    }
}

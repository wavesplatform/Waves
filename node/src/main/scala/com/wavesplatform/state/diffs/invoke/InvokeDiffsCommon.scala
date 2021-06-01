package com.wavesplatform.state.diffs.invoke

import scala.util.{Failure, Right, Success, Try}

import cats.implicits._
import com.google.common.base.Throwables
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, SynchronousCalls}
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.features.InvokeScriptSelfPaymentPolicyProvider._
import com.wavesplatform.features.ScriptTransferValidationProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.{FUNCTION_CALL, _}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{AssetTransfer, _}
import com.wavesplatform.lang.v1.traits.domain.Tx.{BurnPseudoTx, ReissuePseudoTx, ScriptTransfer, SponsorFeePseudoTx}
import com.wavesplatform.settings.Constants
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.DiffsCommon
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.{Asset, AssetIdLength}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, TracedResult}
import com.wavesplatform.transaction.smart.script.trace.AssetVerifierTrace.AssetContext
import com.wavesplatform.transaction.validation.impl.{LeaseCancelTxValidator, LeaseTxValidator, SponsorFeeTxValidator}
import com.wavesplatform.utils._
import shapeless.Coproduct

object InvokeDiffsCommon {
  def txFeeDiff(blockchain: Blockchain, tx: InvokeScriptTransaction): Either[GenericError, (Long, Map[Address, Portfolio])] = {
    val attachedFee = tx.fee
    tx.assetFee._1 match {
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
  }

  def calcAndCheckFee[E <: ValidationError](
      makeError: (String, Long) => E,
      tx: InvokeScriptTransaction,
      blockchain: Blockchain,
      stepLimit: Long,
      invocationComplexity: Long,
      issueList: List[Issue],
      additionalScriptsInvoked: Int
  ): TracedResult[ValidationError, (Long, Map[Address, Portfolio])] =
    TracedResult {
      val stepsNumber =
        if (invocationComplexity % stepLimit == 0)
          invocationComplexity / stepLimit
        else
          invocationComplexity / stepLimit + 1

      for {
        (attachedFeeInWaves, portfolioDiff) <- txFeeDiff(blockchain, tx)
        _ <- {
          val dAppFee    = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit * stepsNumber
          val issuesFee  = issueList.count(!blockchain.isNFT(_)) * FeeConstants(IssueTransaction.typeId) * FeeUnit
          val actionsFee = additionalScriptsInvoked * ScriptExtraFee
          val minFee     = dAppFee + issuesFee + actionsFee

          lazy val errorMessage = {
            val stepsInfo =
              if (stepsNumber > 1)
                s" with $stepsNumber invocation steps"
              else
                ""

            val totalScriptsInvokedInfo =
              if (additionalScriptsInvoked > 0)
                s" with $additionalScriptsInvoked total scripts invoked"
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
              s"$stepsInfo$totalScriptsInvokedInfo$issuesInfo " +
              s"does not exceed minimal value of $minFee WAVES."
          }

          Either.cond(
            attachedFeeInWaves >= minFee,
            (),
            makeError(errorMessage, invocationComplexity)
          )
        }
      } yield (attachedFeeInWaves, portfolioDiff)
    }

  def getInvocationComplexity(
      blockchain: Blockchain,
      call: FUNCTION_CALL,
      callableComplexities: Map[Int, Map[String, Long]],
      dAppAddress: Address
  ): Either[ValidationError, Long] = {
    for {
      complexitiesByCallable <- callableComplexities.get(blockchain.estimator.version).toRight {
        GenericError(s"Cannot find complexity storage, address = $dAppAddress, estimator version = ${blockchain.estimator.version}")
      }
      complexity <- complexitiesByCallable.get(call.function.funcName).toRight {
        GenericError(s"Cannot find callable function `${call.function.funcName}`, address = $dAppAddress`")
      }
    } yield complexity
  }

  def processActions(
      actions: List[CallableAction],
      version: StdLibVersion,
      dAppAddress: Address,
      dAppPublicKey: PublicKey,
      storingComplexity: Int,
      tx: InvokeScriptLike,
      blockchain: Blockchain,
      blockTime: Long,
      isSyncCall: Boolean,
      limitedExecution: Boolean,
      totalComplexityLimit: Int,
      otherIssues: Seq[Issue]
  ): TracedResult[ValidationError, Diff] = {
    val complexityLimit =
      if (limitedExecution) ContractLimits.FailFreeInvokeComplexity - storingComplexity
      else Int.MaxValue

    val actionsByType   = actions.groupBy(a => if (classOf[DataOp].isAssignableFrom(a.getClass)) classOf[DataOp] else a.getClass).withDefaultValue(Nil)
    val transferList    = actionsByType(classOf[AssetTransfer]).asInstanceOf[List[AssetTransfer]]
    val issueList       = actionsByType(classOf[Issue]).asInstanceOf[List[Issue]]
    val reissueList     = actionsByType(classOf[Reissue]).asInstanceOf[List[Reissue]]
    val burnList        = actionsByType(classOf[Burn]).asInstanceOf[List[Burn]]
    val sponsorFeeList  = actionsByType(classOf[SponsorFee]).asInstanceOf[List[SponsorFee]]
    val leaseList       = actionsByType(classOf[Lease]).asInstanceOf[List[Lease]]
    val leaseCancelList = actionsByType(classOf[LeaseCancel]).asInstanceOf[List[LeaseCancel]]
    val dataEntries     = actionsByType(classOf[DataOp]).asInstanceOf[List[DataOp]].map(dataItemToEntry)

    for {
      _ <- TracedResult(checkDataEntries(tx, dataEntries, version)).leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))
      _ <- TracedResult(checkLeaseCancels(leaseCancelList)).leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))
      _ <- TracedResult(
        Either.cond(
          actions.length - dataEntries.length <= ContractLimits.MaxCallableActionsAmount(version),
          (),
          FailedTransactionError.dAppExecution(
            s"Too many script actions: max: ${ContractLimits.MaxCallableActionsAmount(version)}, actual: ${actions.length}",
            storingComplexity
          )
        )
      )

      _ <- TracedResult(checkSelfPayments(dAppAddress, blockchain, tx, version, transferList))
        .leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))
      _ <- TracedResult(
        Either.cond(transferList.map(_.amount).forall(_ >= 0), (), FailedTransactionError.dAppExecution("Negative amount", storingComplexity))
      )
      _ <- TracedResult(checkOverflow(transferList.map(_.amount))).leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))

      actionAssets = transferList.flatMap(_.assetId).map(IssuedAsset) ++
        reissueList.map(r => IssuedAsset(r.assetId)) ++
        burnList.map(b => IssuedAsset(b.assetId)) ++
        sponsorFeeList.map(sf => IssuedAsset(sf.assetId))

      actionComplexities     = actionAssets.flatMap(blockchain.assetScript(_).map(_.complexity))
      verifierCount          = if (blockchain.hasPaidVerifier(tx.senderAddress)) 1 else 0
      additionalScriptsCount = actionComplexities.size + verifierCount + tx.checkedAssets.count(blockchain.hasAssetScript)

      feeDiff <- if (isSyncCall)
        TracedResult.wrapValue(Map[Address, Portfolio]())
      else {
        val feeActionsCount = if (blockchain.isFeatureActivated(SynchronousCalls)) verifierCount else additionalScriptsCount
        val stepLimit       = ContractLimits.MaxComplexityByVersion(version)
        tx.root
          .map(
            calcAndCheckFee(
              FailedTransactionError.feeForActions,
              _,
              blockchain,
              stepLimit,
              storingComplexity.min(stepLimit), // complexity increased by sync calls should not require fee for additional steps
              issueList ++ otherIssues,
              feeActionsCount
            ).map(_._2)
          )
          .getOrElse(TracedResult(Right(Map[Address, Portfolio]())))
      }

      _ <- TracedResult(
        Either.cond(
          actionComplexities.sum + storingComplexity <= totalComplexityLimit || limitedExecution, // limited execution has own restriction "complexityLimit"
          (),
          FailedTransactionError.feeForActions(s"Invoke complexity limit = $totalComplexityLimit is exceeded", storingComplexity)
        )
      )

      paymentsAndFeeDiff = if (isSyncCall) {
        Diff.empty
      } else if (version < V5) {
        paymentsPart(tx, dAppAddress, feeDiff)
      } else {
        Diff(portfolios = txFeeDiff(blockchain, tx.root.get).explicitGet()._2)
      }

      compositeDiff <- foldActions(blockchain, blockTime, tx, dAppAddress, dAppPublicKey)(actions, paymentsAndFeeDiff, complexityLimit)
        .leftMap(_.addComplexity(storingComplexity))

      isr = InvokeScriptResult(
        dataEntries,
        transferList.map { tr =>
          InvokeScriptResult.Payment(
            Address.fromBytes(tr.address.bytes.arr).explicitGet(),
            Asset.fromCompatId(tr.assetId),
            tr.amount
          )
        },
        issueList,
        reissueList,
        burnList,
        sponsorFeeList,
        leaseList.map {
          case l @ Lease(recipient, amount, nonce) =>
            val id = Lease.calculateId(l, tx.txId)
            InvokeScriptResult.Lease(AddressOrAlias.fromRide(recipient).explicitGet(), amount, nonce, id)
        },
        leaseCancelList
      )

      resultDiff = compositeDiff.copy(
        scriptsRun = if (isSyncCall) 0 else additionalScriptsCount + 1,
        scriptResults = Map(tx.txId -> isr),
        scriptsComplexity = storingComplexity + compositeDiff.scriptsComplexity
      )
    } yield resultDiff
  }

  def paymentsPart(tx: InvokeScriptLike, dAppAddress: Address, feePart: Map[Address, Portfolio]): Diff =
    Diff(
      portfolios = feePart |+| tx.payments
        .map {
          case InvokeScriptTransaction.Payment(amt, assetId) =>
            assetId match {
              case asset @ IssuedAsset(_) =>
                Map(tx.senderAddress -> Portfolio(assets = Map(asset -> -amt))) |+|
                  Map(dAppAddress    -> Portfolio(assets = Map(asset -> amt)))
              case Waves =>
                Map(tx.senderAddress -> Portfolio(-amt)) |+|
                  Map(dAppAddress    -> Portfolio(amt))
            }
        }
        .foldLeft(Map[Address, Portfolio]())(_ |+| _)
    )

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
      tx: InvokeScriptLike,
      version: StdLibVersion,
      transfers: List[AssetTransfer]
  ): Either[String, Unit] =
    if (blockchain.disallowSelfPayment && version >= V4)
      if (tx.payments.nonEmpty && tx.senderAddress == dAppAddress)
        "DApp self-payment is forbidden since V4".asLeft[Unit]
      else if (transfers.exists(_.address.bytes == ByteStr(dAppAddress.bytes)))
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

  def checkAsset(blockchain: Blockchain, assetId: ByteStr): Either[String, Unit] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls))
      if (assetId.size != AssetIdLength)
        Left(s"Transfer error: invalid asset ID '$assetId' length = ${assetId.size} bytes, must be $AssetIdLength")
      else if (blockchain.assetDescription(IssuedAsset(assetId)).isEmpty)
        Left(s"Transfer error: asset '$assetId' is not found on the blockchain")
      else
        Right(())
    else
      Right(())

  private[this] def checkDataEntries(
      tx: InvokeScriptLike,
      dataEntries: Seq[DataEntry[_]],
      stdLibVersion: StdLibVersion
  ): Either[String, Unit] =
    for {
      _ <- Either.cond(
        dataEntries.length <= ContractLimits.MaxWriteSetSize(stdLibVersion),
        (),
        s"WriteSet can't contain more than ${ContractLimits.MaxWriteSetSize(stdLibVersion)} entries"
      )
      _ <- Either.cond(
        !tx.enableEmptyKeys || dataEntries.forall(_.key.nonEmpty),
        (),
        s"Empty keys aren't allowed in tx version >= ${tx.root.get.protobufVersion}"
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

  private def checkLeaseCancels(leaseCancels: Seq[LeaseCancel]): Either[String, Unit] = {
    val duplicates = leaseCancels.diff(leaseCancels.distinct)
    Either.cond(
      duplicates.isEmpty,
      (),
      s"Duplicate LeaseCancel id(s): ${duplicates.distinct.map(_.id).mkString(", ")}"
    )
  }

  private def foldActions(
      sblockchain: Blockchain,
      blockTime: Long,
      tx: InvokeScriptLike,
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
          val actionSender = Recipient.Address(ByteStr(tx.dAppAddressOrAlias.bytes)) // XXX Is it correct for aliases&

          def applyTransfer(transfer: AssetTransfer, pk: PublicKey): TracedResult[FailedTransactionError, Diff] = {
            val AssetTransfer(addressRepr, recipient, amount, asset) = transfer
            val address                                              = Address.fromBytes(addressRepr.bytes.arr).explicitGet()
            Asset.fromCompatId(asset) match {
              case Waves =>
                TracedResult.wrapValue(Diff(portfolios = Map(address -> Portfolio(amount)) |+| Map(dAppAddress -> Portfolio(-amount))))
              case a @ IssuedAsset(id) =>
                val nextDiff = Diff(
                  portfolios = Map(address -> Portfolio(assets = Map(a -> amount))) |+| Map(dAppAddress -> Portfolio(assets = Map(a -> -amount)))
                )
                blockchain
                  .assetScript(a)
                  .fold {
                    val r = checkAsset(blockchain, id)
                      .map(_ => nextDiff)
                      .leftMap(FailedTransactionError.dAppExecution(_, 0))
                    TracedResult(r)
                  } {
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
                      val pseudoTxRecipient =
                        if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls))
                          recipient
                        else
                          Recipient.Address(addressRepr.bytes)
                      val pseudoTx = ScriptTransfer(
                        asset,
                        actionSender,
                        pk,
                        pseudoTxRecipient,
                        amount,
                        tx.timestamp,
                        tx.txId
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
            TracedResult.wrapValue(Diff(accountData = Map(dAppAddress -> AccountDataInfo(Map(item.key -> dataItemToEntry(item))))))

          def applyIssue(itx: InvokeScriptLike, pk: PublicKey, issue: Issue): TracedResult[FailedTransactionError, Diff] = {
            if (issue.name
                  .getBytes("UTF-8")
                  .length < IssueTransaction.MinAssetNameLength || issue.name.getBytes("UTF-8").length > IssueTransaction.MaxAssetNameLength) {
              TracedResult(Left(FailedTransactionError.dAppExecution("Invalid asset name", 0L)), List())
            } else if (issue.description.length > IssueTransaction.MaxAssetDescriptionLength) {
              TracedResult(Left(FailedTransactionError.dAppExecution("Invalid asset description", 0L)), List())
            } else if (blockchain.assetDescription(IssuedAsset(issue.id)).isDefined) {
              TracedResult(Left(FailedTransactionError.dAppExecution(s"Asset ${issue.id} is already issued", 0L)), List())
            } else {
              val staticInfo = AssetStaticInfo(TransactionId @@ itx.txId, pk, issue.decimals, blockchain.isNFT(issue))
              val volumeInfo = AssetVolumeInfo(issue.isReissuable, BigInt(issue.quantity))
              val info       = AssetInfo(ByteString.copyFromUtf8(issue.name), ByteString.copyFromUtf8(issue.description), Height @@ blockchain.height)

              val asset = IssuedAsset(issue.id)

              DiffsCommon
                .countVerifierComplexity(None /*issue.compiledScript*/, blockchain, isAsset = true)
                .map { script =>
                  Diff(
                    portfolios = Map(pk.toAddress -> Portfolio(assets = Map(asset -> issue.quantity))),
                    issuedAssets = Map(asset      -> NewAssetInfo(staticInfo, info, volumeInfo)),
                    assetScripts = Map(asset      -> script.map(script => AssetScriptInfo(script._1, script._2)))
                  )
                }
                .leftMap(FailedTransactionError.asFailedScriptError)
            }
          }

          def applyReissue(reissue: Reissue, pk: PublicKey): TracedResult[FailedTransactionError, Diff] = {
            val reissueDiff = DiffsCommon.processReissue(blockchain, dAppAddress, blockTime, fee = 0, reissue).leftMap(FailedTransactionError.asFailedScriptError)
            val pseudoTx    = ReissuePseudoTx(reissue, actionSender, pk, tx.txId, tx.timestamp)
            callAssetVerifierWithPseudoTx(reissueDiff, reissue.assetId, pseudoTx, AssetContext.Reissue)
          }

          def applyBurn(burn: Burn, pk: PublicKey): TracedResult[FailedTransactionError, Diff] = {
            val burnDiff = DiffsCommon.processBurn(blockchain, dAppAddress, fee = 0, burn).leftMap(FailedTransactionError.asFailedScriptError)
            val pseudoTx = BurnPseudoTx(burn, actionSender, pk, tx.txId, tx.timestamp)
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
              _ <- TracedResult(SponsorFeeTxValidator.checkMinSponsoredAssetFee(sponsorFee.minSponsoredAssetFee).leftMap(FailedTransactionError.asFailedScriptError))
              sponsorDiff = DiffsCommon.processSponsor(blockchain, dAppAddress, fee = 0, sponsorFee).leftMap(FailedTransactionError.asFailedScriptError)
              pseudoTx    = SponsorFeePseudoTx(sponsorFee, actionSender, pk, tx.txId, tx.timestamp)
              r <- callAssetVerifierWithPseudoTx(sponsorDiff, sponsorFee.assetId, pseudoTx, AssetContext.Sponsor)
            } yield r

          def applyLease(l: Lease): TracedResult[ValidationError, Diff] =
            for {
              _         <- TracedResult(LeaseTxValidator.validateAmount(l.amount))
              recipient <- TracedResult(AddressOrAlias.fromRide(l.recipient))
              leaseId = Lease.calculateId(l, tx.txId)
              diff <- DiffsCommon.processLease(blockchain, l.amount, pk, recipient, fee = 0, leaseId, tx.txId)
            } yield diff

          def applyLeaseCancel(l: LeaseCancel): TracedResult[ValidationError, Diff] =
            for {
              _    <- TracedResult(LeaseCancelTxValidator.checkLeaseId(l.id))
              diff <- DiffsCommon.processLeaseCancel(blockchain, pk, fee = 0, blockTime, l.id, tx.txId)
            } yield diff

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
            case d: DataOp       => applyDataItem(d)
            case i: Issue        => applyIssue(tx, pk, i)
            case r: Reissue      => applyReissue(r, pk)
            case b: Burn         => applyBurn(b, pk)
            case sf: SponsorFee  => applySponsorFee(sf, pk)
            case l: Lease        => applyLease(l).leftMap(FailedTransactionError.asFailedScriptError)
            case lc: LeaseCancel => applyLeaseCancel(lc).leftMap(FailedTransactionError.asFailedScriptError)
          }
          diffAcc |+| diff.leftMap(_.addComplexity(curDiff.scriptsComplexity))

        case _ => diffAcc
      }
    }

  private def validatePseudoTxWithSmartAssetScript(blockchain: Blockchain, tx: InvokeScriptLike)(
      pseudoTx: PseudoTx,
      assetId: ByteStr,
      nextDiff: Diff,
      script: Script,
      estimatedComplexity: Long,
      complexityLimit: Int
  ): Either[FailedTransactionError, Diff] =
    Try {
      val (log, evaluatedComplexity, result) = ScriptRunner(
        Coproduct[TxOrd](pseudoTx),
        blockchain,
        script,
        isAssetScript = true,
        scriptContainerAddress =
          if (blockchain.passCorrectAssetId) Coproduct[Environment.Tthis](Environment.AssetId(assetId.arr))
          else Coproduct[Environment.Tthis](Environment.AssetId(tx.dAppAddressOrAlias.bytes)),
        complexityLimit
      )
      val complexity = if (blockchain.storeEvaluatedComplexity) evaluatedComplexity else estimatedComplexity
      result match {
        case Left(error)  => Left(FailedTransactionError.assetExecutionInAction(error, complexity, log, assetId))
        case Right(FALSE) => Left(FailedTransactionError.notAllowedByAssetInAction(complexity, log, assetId))
        case Right(TRUE)  => Right(nextDiff.copy(scriptsComplexity = nextDiff.scriptsComplexity + complexity))
        case Right(x) =>
          Left(FailedTransactionError.assetExecutionInAction(s"Script returned not a boolean result, but $x", complexity, log, assetId))
      }
    } match {
      case Failure(e) =>
        Left(
          FailedTransactionError
            .assetExecutionInAction(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", estimatedComplexity, List.empty, assetId)
        )
      case Success(s) => s
    }

  case class StepInfo(
      feeInWaves: Long,
      feeInAttachedAsset: Long,
      scriptsRun: Int
  )
}

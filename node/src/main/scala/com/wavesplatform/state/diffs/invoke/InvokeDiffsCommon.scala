package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits.*
import com.google.common.base.Throwables
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, RideV6, SynchronousCalls}
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.features.InvokeScriptSelfPaymentPolicyProvider.*
import com.wavesplatform.features.ScriptTransferValidationProvider.*
import com.wavesplatform.lang.*
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.{FUNCTION_CALL, *}
import com.wavesplatform.lang.v1.evaluator.{Log, ScriptResult, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.lang.v1.traits.domain.Tx.{BurnPseudoTx, ReissuePseudoTx, ScriptTransfer, SponsorFeePseudoTx}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.FeeValidation.*
import com.wavesplatform.state.diffs.{BalanceDiffValidation, DiffsCommon}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.AssetVerifierTrace.AssetContext
import com.wavesplatform.transaction.smart.script.trace.TracedResult.Attribute
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, TracedResult}
import com.wavesplatform.transaction.validation.impl.{DataTxValidator, LeaseCancelTxValidator, LeaseTxValidator, SponsorFeeTxValidator}
import com.wavesplatform.transaction.{Asset, AssetIdLength, ERC20Address, PBSince, TransactionType}
import com.wavesplatform.utils.*
import shapeless.Coproduct

import scala.util.{Failure, Right, Success, Try}

object InvokeDiffsCommon {
  val callExpressionError: Either[GenericError, Nothing] =
    Left(GenericError("Trying to call dApp on the account with expression script"))

  def txFeeDiff(blockchain: Blockchain, tx: InvokeScriptTransactionLike): Either[GenericError, (Long, Map[Address, Portfolio])] = {
    val attachedFee = tx.fee
    tx.feeAssetId match {
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
          portfolioDiff <- Diff
            .combine(
              Map[Address, Portfolio](tx.sender.toAddress        -> Portfolio.build(asset, -attachedFee)),
              Map[Address, Portfolio](assetInfo.issuer.toAddress -> Portfolio.build(-feeInWaves, asset, attachedFee))
            )
            .leftMap(GenericError(_))
        } yield (feeInWaves, portfolioDiff)
    }
  }

  private def calculateMinFee(
      tx: InvokeScriptTransactionLike,
      blockchain: Blockchain,
      issueList: List[Issue],
      additionalScriptsInvoked: Int,
      stepsNumber: Long
  ): Long = {
    val dAppFee    = FeeConstants(tx.tpe) * FeeUnit * stepsNumber
    val issuesFee  = issueList.count(!blockchain.isNFT(_)) * FeeConstants(TransactionType.Issue) * FeeUnit
    val actionsFee = additionalScriptsInvoked * ScriptExtraFee
    dAppFee + issuesFee + actionsFee
  }

  private[invoke] def calcAndCheckFee[E <: ValidationError](
      makeError: (String, Long) => E,
      tx: InvokeScriptTransactionLike,
      blockchain: Blockchain,
      stepLimit: Long,
      invocationComplexity: Long,
      issueList: List[Issue],
      additionalScriptsInvoked: Int
  ): TracedResult[ValidationError, (Long, Map[Address, Portfolio])] = {
    val stepsNumber =
      if (invocationComplexity % stepLimit == 0)
        invocationComplexity / stepLimit
      else
        invocationComplexity / stepLimit + 1

    val minFee = calculateMinFee(tx, blockchain, issueList, additionalScriptsInvoked, stepsNumber)

    val resultE = for {
      (attachedFeeInWaves, portfolioDiff) <- txFeeDiff(blockchain, tx)
      _ <- {
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
          val txName    = tx.tpe.transactionName

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
    TracedResult(resultE).withAttributes(Attribute.MinFee -> minFee)
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

    val actionsByType  = actions.groupBy(a => if (classOf[DataOp].isAssignableFrom(a.getClass)) classOf[DataOp] else a.getClass).withDefaultValue(Nil)
    val transferList   = actionsByType(classOf[AssetTransfer]).asInstanceOf[List[AssetTransfer]]
    val issueList      = actionsByType(classOf[Issue]).asInstanceOf[List[Issue]]
    val reissueList    = actionsByType(classOf[Reissue]).asInstanceOf[List[Reissue]]
    val burnList       = actionsByType(classOf[Burn]).asInstanceOf[List[Burn]]
    val sponsorFeeList = actionsByType(classOf[SponsorFee]).asInstanceOf[List[SponsorFee]]
    val leaseList      = actionsByType(classOf[Lease]).asInstanceOf[List[Lease]]
    val leaseCancelList = actionsByType(classOf[LeaseCancel]).asInstanceOf[List[LeaseCancel]]
    val dataEntries     = actionsByType(classOf[DataOp]).asInstanceOf[List[DataOp]].map(dataItemToEntry)

    for {
      _ <- TracedResult(checkDataEntries(blockchain, tx, dataEntries, version)).leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))
      _ <- TracedResult(checkLeaseCancels(leaseCancelList)).leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))
      _ <- TracedResult(
        checkScriptActionsAmount(version, actions, transferList, leaseList, leaseCancelList, dataEntries)
          .leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))
      )

      _ <- TracedResult(checkSelfPayments(dAppAddress, blockchain, tx, version, transferList))
        .leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))
      _ <- TracedResult(
        Either.cond(transferList.map(_.amount).forall(_ >= 0), (), FailedTransactionError.dAppExecution("Negative amount", storingComplexity))
      )
      _ <- TracedResult(checkOverflow(transferList.map(_.amount))).leftMap(FailedTransactionError.dAppExecution(_, storingComplexity))

      actionAssets = transferList.flatMap(_.assetId).map(IssuedAsset(_)) ++
        reissueList.map(r => IssuedAsset(r.assetId)) ++
        burnList.map(b => IssuedAsset(b.assetId)) ++
        sponsorFeeList.map(sf => IssuedAsset(sf.assetId))

      actionComplexities     = actionAssets.flatMap(blockchain.assetScript(_).map(_.complexity))
      verifierCount          = if (blockchain.hasPaidVerifier(tx.sender.toAddress)) 1 else 0
      additionalScriptsCount = actionComplexities.size + verifierCount + tx.paymentAssets.count(blockchain.hasAssetScript)

      feeDiff <-
        if (isSyncCall)
          TracedResult.wrapValue(Map[Address, Portfolio]())
        else {
          val feeActionsCount = if (blockchain.isFeatureActivated(SynchronousCalls)) verifierCount else additionalScriptsCount
          val stepLimit       = ContractLimits.MaxComplexityByVersion(version)

          calcAndCheckFee(
            FailedTransactionError.feeForActions,
            tx.root,
            blockchain,
            stepLimit,
            storingComplexity.min(stepLimit), // complexity increased by sync calls should not require fee for additional steps
            issueList ++ otherIssues,
            feeActionsCount
          ).map(_._2)

        }

      _ <- TracedResult(
        Either.cond(
          actionComplexities.sum + storingComplexity <= totalComplexityLimit || limitedExecution, // limited execution has own restriction "complexityLimit"
          (),
          FailedTransactionError.feeForActions(s"Invoke complexity limit = $totalComplexityLimit is exceeded", storingComplexity)
        )
      )

      paymentsAndFeeDiff <-
        if (isSyncCall) {
          TracedResult.wrapValue(Diff.empty)
        } else if (version < V5) {
          TracedResult(paymentsPart(tx, dAppAddress, feeDiff))
        } else {
          TracedResult.wrapValue(Diff(portfolios = txFeeDiff(blockchain, tx.root).explicitGet()._2))
        }

      resultTransfers <- transferList.traverse { transfer =>
        resolveAddress(transfer.address, blockchain)
          .map(InvokeScriptResult.Payment(_, Asset.fromCompatId(transfer.assetId), transfer.amount))
          .leftMap {
            case f: FailedTransactionError => f.addComplexity(storingComplexity)
            case e                         => e
          }
      }

      compositeDiff <- foldActions(blockchain, blockTime, tx, dAppAddress, dAppPublicKey)(actions, paymentsAndFeeDiff, complexityLimit)
        .leftMap {
          case failed: FailedTransactionError => failed.addComplexity(storingComplexity)
          case other                          => other
        }

      isr = InvokeScriptResult(
        dataEntries,
        resultTransfers,
        issueList,
        reissueList,
        burnList,
        sponsorFeeList,
        leaseList.map { case l @ Lease(recipient, amount, nonce) =>
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

  def paymentsPart(tx: InvokeScriptLike, dAppAddress: Address, feePart: Map[Address, Portfolio]): Either[GenericError, Diff] =
    tx.payments
      .traverse { case InvokeScriptTransaction.Payment(amt, assetId) =>
        assetId match {
          case asset @ IssuedAsset(_) =>
            Diff.combine(
              Map(tx.sender.toAddress -> Portfolio.build(asset, -amt)),
              Map(dAppAddress         -> Portfolio.build(asset, amt))
            )
          case Waves =>
            Diff.combine(
              Map(tx.sender.toAddress -> Portfolio(-amt)),
              Map(dAppAddress         -> Portfolio(amt))
            )
        }
      }
      .flatMap(_.foldM(Map[Address, Portfolio]())(Diff.combine))
      .flatMap(Diff.combine(feePart, _))
      .bimap(GenericError(_), p => Diff(portfolios = p))

  def dataItemToEntry(item: DataOp): DataEntry[?] =
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
      if (tx.payments.nonEmpty && tx.sender.toAddress == dAppAddress)
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
        _ => "ScriptTransfer overflow".asLeft[Unit],
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

  private def checkDataEntries(blockchain: Blockchain, tx: InvokeScriptLike, dataEntries: Seq[DataEntry[?]], stdLibVersion: StdLibVersion) =
    for {
      _ <- Either.cond(
        dataEntries.length <= ContractLimits.MaxWriteSetSize,
        (),
        s"WriteSet can't contain more than ${ContractLimits.MaxWriteSetSize} entries"
      )
      _ <- Either.cond(
        tx.enableEmptyKeys || dataEntries.forall(_.key.nonEmpty),
        (), {
          val versionInfo = tx.root match {
            case s: PBSince => s" in tx version >= ${s.protobufVersion}"
            case _          => ""
          }
          s"Empty keys aren't allowed$versionInfo"
        }
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

      _ <- DataTxValidator.verifyInvokeWriteSet(blockchain, dataEntries)
    } yield ()

  private def checkLeaseCancels(leaseCancels: Seq[LeaseCancel]): Either[String, Unit] = {
    val duplicates = leaseCancels.diff(leaseCancels.distinct)
    Either.cond(
      duplicates.isEmpty,
      (),
      s"Duplicate LeaseCancel id(s): ${duplicates.distinct.map(_.id).mkString(", ")}"
    )
  }

  private def checkScriptActionsAmount(
      version: StdLibVersion,
      actions: List[CallableAction],
      transferList: List[AssetTransfer],
      leaseList: List[Lease],
      leaseCancelList: List[LeaseCancel],
      dataEntries: Seq[DataEntry[?]]
  ): Either[String, Unit] = {
    if (version >= V6) {
      val balanceChangeActionsAmount = transferList.length + leaseList.length + leaseCancelList.length
      val assetsActionsAmount        = actions.length - dataEntries.length - balanceChangeActionsAmount

      for {
        _ <- Either.cond(
          balanceChangeActionsAmount <= ContractLimits.MaxBalanceScriptActionsAmountV6,
          (),
          s"Too many ScriptTransfer, Lease, LeaseCancel actions: max: ${ContractLimits.MaxBalanceScriptActionsAmountV6}, actual: $balanceChangeActionsAmount"
        )
        _ <- Either.cond(
          assetsActionsAmount <= ContractLimits.MaxAssetScriptActionsAmountV6,
          (),
          s"Too many Issue, Reissue, Burn, SponsorFee actions: max: ${ContractLimits.MaxAssetScriptActionsAmountV6}, actual: $assetsActionsAmount"
        )
      } yield ()
    } else {
      val actionsAmount = actions.length - dataEntries.length

      Either.cond(
        actionsAmount <= ContractLimits.MaxCallableActionsAmountBeforeV6(version),
        (),
        s"Too many script actions: max: ${ContractLimits.MaxCallableActionsAmountBeforeV6(version)}, actual: $actionsAmount"
      )
    }
  }

  private def resolveAddress(recipient: Recipient.Address, blockchain: Blockchain): TracedResult[ValidationError, Address] =
    TracedResult {
      val address = Address.fromBytes(recipient.bytes.arr)
      if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6))
        address.leftMap(e => FailedTransactionError.dAppExecution(e.reason, 0))
      else
        address
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
  ): TracedResult[ValidationError, Diff] =
    actions.foldM(paymentsDiff) { (curDiff, action) =>
      val complexityLimit =
        if (remainingLimit < Int.MaxValue) remainingLimit - curDiff.scriptsComplexity.toInt
        else remainingLimit

      val blockchain   = CompositeBlockchain(sblockchain, curDiff)
      val actionSender = Recipient.Address(ByteStr(dAppAddress.bytes))

      def applyTransfer(transfer: AssetTransfer, pk: PublicKey): TracedResult[ValidationError, Diff] = {
        val AssetTransfer(addressRepr, recipient, amount, asset) = transfer

        for {
          address <- TracedResult(Address.fromBytes(addressRepr.bytes.arr))
          diff <- Asset.fromCompatId(asset) match {
            case Waves =>
              TracedResult(Diff.combine(Map(address -> Portfolio(amount)), Map(dAppAddress -> Portfolio(-amount))).map(p => Diff(portfolios = p)))
                .leftMap(GenericError(_))
            case a @ IssuedAsset(id) =>
              TracedResult(
                Diff
                  .combine(Map(address -> Portfolio(assets = Map(a -> amount))), Map(dAppAddress -> Portfolio(assets = Map(a -> -amount))))
                  .bimap(GenericError(_), p => Diff(portfolios = p))
              ).flatMap(nextDiff =>
                blockchain
                  .assetScript(a)
                  .fold {
                    val r = checkAsset(blockchain, id)
                      .map(_ => nextDiff)
                      .leftMap(FailedTransactionError.dAppExecution(_, 0): ValidationError)
                    TracedResult(r)
                  } { case AssetScriptInfo(script, complexity) =>
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
                    val assetValidationDiff = for {
                      _ <- BalanceDiffValidation.cond(blockchain, _.isFeatureActivated(BlockchainFeatures.RideV6))(assetVerifierDiff)
                      assetValidationDiff <- validatePseudoTxWithSmartAssetScript(blockchain, tx)(
                        pseudoTx,
                        a.id,
                        assetVerifierDiff,
                        script,
                        complexity,
                        complexityLimit
                      )
                    } yield assetValidationDiff
                    val errorOpt = assetValidationDiff.fold(Some(_), _ => None)
                    TracedResult(
                      assetValidationDiff.map(d => nextDiff.copy(scriptsComplexity = d.scriptsComplexity)),
                      List(AssetVerifierTrace(id, errorOpt, AssetContext.Transfer))
                    )
                  }
              )
          }
        } yield diff
      }

      def applyDataItem(item: DataOp): TracedResult[FailedTransactionError, Diff] =
        TracedResult.wrapValue(Diff(accountData = Map(dAppAddress -> AccountDataInfo(Map(item.key -> dataItemToEntry(item))))))

      def applyIssue(itx: InvokeScriptLike, pk: PublicKey, issue: Issue): TracedResult[ValidationError, Diff] = {
        val asset = IssuedAsset(issue.id)
        if (
          issue.name.getBytes("UTF-8").length < IssueTransaction.MinAssetNameLength ||
          issue.name.getBytes("UTF-8").length > IssueTransaction.MaxAssetNameLength
        ) {
          TracedResult(Left(FailedTransactionError.dAppExecution("Invalid asset name", 0L)), List())
        } else if (issue.description.length > IssueTransaction.MaxAssetDescriptionLength) {
          TracedResult(Left(FailedTransactionError.dAppExecution("Invalid asset description", 0L)), List())
        } else if (blockchain.assetDescription(IssuedAsset(issue.id)).isDefined || blockchain.resolveERC20Address(ERC20Address(asset)).isDefined) {
          val error = s"Asset ${issue.id} is already issued"
          if (blockchain.isFeatureActivated(RideV6) || blockchain.height < blockchain.settings.functionalitySettings.enforceTransferValidationAfter) {
            TracedResult(Left(FailedTransactionError.dAppExecution(error, 0L)), List())
          } else {
            TracedResult(Left(FailOrRejectError(error)))
          }
        } else {
          val staticInfo = AssetStaticInfo(TransactionId @@ itx.txId, pk, issue.decimals, blockchain.isNFT(issue))
          val volumeInfo = AssetVolumeInfo(issue.isReissuable, BigInt(issue.quantity))
          val info       = AssetInfo(ByteString.copyFromUtf8(issue.name), ByteString.copyFromUtf8(issue.description), Height @@ blockchain.height)
          Right(
            Diff(
              portfolios = Map(pk.toAddress -> Portfolio(assets = Map(asset -> issue.quantity))),
              issuedAssets = Map(asset -> NewAssetInfo(staticInfo, info, volumeInfo)),
              assetScripts = Map(asset -> None)
            )
          )
        }
      }

      def applyReissue(reissue: Reissue, pk: PublicKey): TracedResult[ValidationError, Diff] = {
        val reissueDiff =
          DiffsCommon.processReissue(blockchain, dAppAddress, blockTime, fee = 0, reissue).leftMap(FailedTransactionError.asFailedScriptError)
        val pseudoTx = ReissuePseudoTx(reissue, actionSender, pk, tx.txId, tx.timestamp)
        callAssetVerifierWithPseudoTx(reissueDiff, reissue.assetId, pseudoTx, AssetContext.Reissue)
      }

      def applyBurn(burn: Burn, pk: PublicKey): TracedResult[ValidationError, Diff] = {
        val burnDiff = DiffsCommon.processBurn(blockchain, dAppAddress, fee = 0, burn).leftMap(FailedTransactionError.asFailedScriptError)
        val pseudoTx = BurnPseudoTx(burn, actionSender, pk, tx.txId, tx.timestamp)
        callAssetVerifierWithPseudoTx(burnDiff, burn.assetId, pseudoTx, AssetContext.Burn)
      }

      def applySponsorFee(sponsorFee: SponsorFee, pk: PublicKey): TracedResult[ValidationError, Diff] =
        for {
          _ <- TracedResult(
            Either.cond(
              blockchain.assetDescription(IssuedAsset(sponsorFee.assetId)).exists(_.issuer == pk),
              (),
              FailedTransactionError.dAppExecution(s"SponsorFee assetId=${sponsorFee.assetId} was not issued from address of current dApp", 0L)
            )
          )
          _ <- TracedResult(
            SponsorFeeTxValidator.checkMinSponsoredAssetFee(sponsorFee.minSponsoredAssetFee).leftMap(FailedTransactionError.asFailedScriptError)
          )
          sponsorDiff = DiffsCommon
            .processSponsor(blockchain, dAppAddress, fee = 0, sponsorFee)
            .leftMap(FailedTransactionError.asFailedScriptError)
          pseudoTx = SponsorFeePseudoTx(sponsorFee, actionSender, pk, tx.txId, tx.timestamp)
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
      ): TracedResult[ValidationError, Diff] =
        blockchain.assetScript(IssuedAsset(assetId)).fold(TracedResult(actionDiff)) { case AssetScriptInfo(script, complexity) =>
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

      val nextDiff = action match {
        case t: AssetTransfer =>
          applyTransfer(
            t,
            if (blockchain.isFeatureActivated(BlockV5)) {
              pk
            } else {
              PublicKey(new Array[Byte](32))
            }
          )
        case d: DataOp       => applyDataItem(d)
        case i: Issue        => applyIssue(tx, pk, i)
        case r: Reissue      => applyReissue(r, pk)
        case b: Burn         => applyBurn(b, pk)
        case sf: SponsorFee  => applySponsorFee(sf, pk)
        case l: Lease        => applyLease(l).leftMap(FailedTransactionError.asFailedScriptError)
        case lc: LeaseCancel => applyLeaseCancel(lc).leftMap(FailedTransactionError.asFailedScriptError)
      }
      nextDiff
        .flatMap(baseDiff =>
          TracedResult(
            BalanceDiffValidation
              .cond(blockchain, _.isFeatureActivated(BlockchainFeatures.RideV6))(baseDiff)
              .leftMap(FailedTransactionError.asFailedScriptError(_).addComplexity(baseDiff.scriptsComplexity))
          )
        )
        .leftMap {
          case f: FailedTransactionError => f.addComplexity(curDiff.scriptsComplexity)
          case e                         => e
        }
        .flatMap(d => TracedResult(curDiff.combineF(d)).leftMap(GenericError(_)))
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
          else Coproduct[Environment.Tthis](Environment.AssetId(tx.dApp.bytes)),
        complexityLimit
      )
      val complexity = if (blockchain.storeEvaluatedComplexity) evaluatedComplexity else estimatedComplexity
      result match {
        case Left(error)  => Left(FailedTransactionError.assetExecutionInAction(error.message, complexity, log, assetId))
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

  def checkCallResultLimits(
      version: StdLibVersion,
      blockchain: Blockchain,
      usedComplexity: Long,
      log: Log[Id],
      actionsCount: Int,
      balanceActionsCount: Int,
      assetActionsCount: Int,
      dataCount: Int,
      dataSize: Int,
      availableActions: Int,
      availableBalanceActions: Int,
      availableAssetActions: Int,
      availableData: Int,
      availableDataSize: Int
  ): TracedResult[ValidationError, Unit] = {
    def error(message: String) = TracedResult(Left(FailedTransactionError.dAppExecution(message, usedComplexity, log)))

    if (dataCount > availableData)
      error("Stored data count limit is exceeded")
    else if (dataSize > availableDataSize) {
      val limit   = ContractLimits.MaxTotalWriteSetSizeInBytes
      val actual  = limit + dataSize - availableDataSize
      val message = s"Storing data size should not exceed $limit, actual: $actual bytes"
      if (blockchain.isFeatureActivated(RideV6)) {
        error(message)
      } else if (
        blockchain.isFeatureActivated(
          SynchronousCalls
        ) && blockchain.height >= blockchain.settings.functionalitySettings.enforceTransferValidationAfter

      ) {
        TracedResult(Left(FailOrRejectError(message)))
      } else
        TracedResult(Right(()))
    } else if (version >= V6 && balanceActionsCount > availableBalanceActions) {
      error("ScriptTransfer, Lease, LeaseCancel actions count limit is exceeded")
    } else if (version >= V6 && assetActionsCount > availableAssetActions) {
      error("Issue, Reissue, Burn, SponsorFee actions count limit is exceeded")
    } else if (version < V6 && actionsCount > availableActions)
      error("Actions count limit is exceeded")
    else
      TracedResult(Right(()))
  }

  def checkScriptResultFields(blockchain: Blockchain, r: ScriptResult): Either[ValidationError, ScriptResult] =
    r match {
      case rv4: ScriptResultV4 =>
        rv4.actions
          .collectFirstSome {
            case Reissue(_, _, quantity) if quantity < 0   => Some(s"Negative reissue quantity = $quantity")
            case Burn(_, quantity) if quantity < 0         => Some(s"Negative burn quantity = $quantity")
            case t: AssetTransfer if t.amount < 0          => Some(s"Negative transfer amount = ${t.amount}")
            case l: Lease if l.amount < 0                  => Some(s"Negative lease amount = ${l.amount}")
            case SponsorFee(_, Some(amount)) if amount < 0 => Some(s"Negative sponsor amount = $amount")
            case i: Issue =>
              val length = i.name.getBytes("UTF-8").length
              if (length < IssueTransaction.MinAssetNameLength || length > IssueTransaction.MaxAssetNameLength)
                Some("Invalid asset name")
              else if (i.description.length > IssueTransaction.MaxAssetDescriptionLength)
                Some("Invalid asset description")
              else
                None
            case _ =>
              None
          }
          .collect {
            case e if blockchain.isFeatureActivated(BlockchainFeatures.RideV6)                                      => GenericError(e)
            case e if blockchain.height >= blockchain.settings.functionalitySettings.enforceTransferValidationAfter => FailOrRejectError(e)
          }
          .toLeft(r)
      case _ => Right(r)
    }
}

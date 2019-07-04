package com.wavesplatform.state.diffs

import cats.implicits._
import cats.kernel.Monoid
import com.google.common.base.Throwables
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, LogItem, ScriptResult}
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient}
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.CommonValidation._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart.BlockchainContext.In
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{AssetVerifierTrace, InvokeScriptTrace, TracedResult}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, WavesEnvironment}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.{Failure, Success, Try}

object InvokeScriptTransactionDiff {

  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(blockchain: Blockchain)(tx: InvokeScriptTransaction): TracedResult[ValidationError, Diff] = {

    val dAppAddressEi = blockchain.resolveAlias(tx.dAppAddressOrAlias)
    val accScriptEi   = dAppAddressEi.map(blockchain.accountScript)
    val functioncall  = tx.funcCall

    accScriptEi match {
      case Right(Some(sc @ ContractScriptImpl(_, contract, _))) =>
        val scriptResultE =
          stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)({
            val environment = new WavesEnvironment(
              AddressScheme.current.chainId,
              Coeval(tx.asInstanceOf[In]),
              Coeval(blockchain.height),
              blockchain,
              Coeval(tx.dAppAddressOrAlias.bytes)
            )
            val invoker                                       = tx.sender.toAddress.bytes
            val maybePayment: Option[(Long, Option[ByteStr])] = tx.payment.headOption.map(p => (p.amount, p.assetId.compatId))
            val invocation = ContractEvaluator.Invocation(
              functioncall,
              Recipient.Address(invoker),
              tx.sender,
              maybePayment,
              tx.dAppAddressOrAlias.bytes,
              tx.id.value,
              tx.fee,
              tx.feeAssetId.compatId
            )
            val result = for {
              directives <- DirectiveSet(V3, Account, DAppType).leftMap((_, List.empty[LogItem]))
              evaluator <- ContractEvaluator(
                Monoid
                  .combineAll(
                    Seq(
                      PureContext.build(Global, V3),
                      CryptoContext.build(Global, V3),
                      WavesContext.build(directives, environment)
                    )
                  )
                  .evaluationContext,
                contract,
                invocation
              )
            } yield evaluator

            result.leftMap { case (error, log) => ScriptExecutionError(error, log, isAssetScript = false) }
          })
        for {
          scriptResult <- TracedResult(scriptResultE, List(InvokeScriptTrace(tx.dAppAddressOrAlias, functioncall, scriptResultE)))
          ScriptResult(ds, ps) = scriptResult

          dataEntries = ds.map {
            case DataItem.Bool(k, b) => BooleanDataEntry(k, b)
            case DataItem.Str(k, b)  => StringDataEntry(k, b)
            case DataItem.Lng(k, b)  => IntegerDataEntry(k, b)
            case DataItem.Bin(k, b)  => BinaryDataEntry(k, b)
          }

          pmts: List[Map[Address, Map[Option[ByteStr], Long]]] = ps.map {
            case (Recipient.Address(addrBytes), amt, maybeAsset) =>
              Map(Address.fromBytes(addrBytes.arr).explicitGet() -> Map(maybeAsset -> amt))
          }

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
              } yield {
                (wavesFee,
                 Map(
                   tx.sender.toAddress        -> Portfolio(0, LeaseBalance.empty, Map(asset         -> -tx.fee)),
                   assetInfo.issuer.toAddress -> Portfolio(-wavesFee, LeaseBalance.empty, Map(asset -> tx.fee))
                 ))
              }
          })
          dAppAddress <- TracedResult(dAppAddressEi)
          wavesFee = feeInfo._1
          dataAndPaymentDiff <- TracedResult(payableAndDataPart(blockchain.height, tx, dAppAddress, dataEntries, feeInfo._2))
          _                  <- TracedResult(Either.cond(pmts.flatMap(_.values).flatMap(_.values).forall(_ >= 0), (), NegativeAmount(-42, "")))
          _                  <- TracedResult(validateOverflow(pmts.flatMap(_.values).flatMap(_.values), "Attempt to transfer unavailable funds in contract payment"))
          _ <- TracedResult(
            Either.cond(
              pmts
                .flatMap(_.values)
                .flatMap(_.keys)
                .flatten
                .forall(id => blockchain.assetDescription(IssuedAsset(id)).isDefined),
              (),
              GenericError(s"Unissued assets are not allowed")
            ))
          _ <- TracedResult {
            val totalScriptsInvoked =
              tx.checkedAssets()
                .collect { case asset @ IssuedAsset(_) => asset }
                .count(blockchain.hasAssetScript) +
                ps.count(_._3.fold(false)(id => blockchain.hasAssetScript(IssuedAsset(id)))) +
                (if (blockchain.hasScript(tx.sender)) 1 else 0)
            val minWaves = totalScriptsInvoked * ScriptExtraFee + FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit
            Either.cond(
              minWaves <= wavesFee,
              (),
              GenericError(s"Fee in ${tx.assetFee._1
                .fold("WAVES")(_.toString)} for ${tx.builder.classTag} with $totalScriptsInvoked total scripts invoked does not exceed minimal value of $minWaves WAVES: ${tx.assetFee._2}")
            )
          }
          scriptsInvoked <- TracedResult {
            val totalScriptsInvoked =
              tx.checkedAssets()
                .collect { case asset @ IssuedAsset(_) => asset }
                .count(blockchain.hasAssetScript) +
                ps.count(_._3.fold(false)(id => blockchain.hasAssetScript(IssuedAsset(id)))) +
                (if (blockchain.hasScript(tx.sender)) { 1 } else { 0 })
            val minWaves = totalScriptsInvoked * ScriptExtraFee + FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit
            Either.cond(
              minWaves <= wavesFee,
              totalScriptsInvoked,
              GenericError(s"Fee in ${tx.assetFee._1
                .fold("WAVES")(_.toString)} for ${tx.builder.classTag} with $totalScriptsInvoked total scripts invoked does not exceed minimal value of $minWaves WAVES: ${tx.assetFee._2}")
            )
          }

          scriptsComplexity = {
            val assetsComplexity = (tx.checkedAssets().map(_.id) ++ ps.flatMap(_._3))
              .flatMap(id => blockchain.assetScript(IssuedAsset(id)))
              .map(DiffsCommon.verifierComplexity)
              .sum

            val accountComplexity = blockchain
              .accountScript(tx.sender)
              .fold(0L)(DiffsCommon.verifierComplexity)

            val funcComplexity = DiffsCommon.functionComplexity(sc, tx.funcCallOpt)

            assetsComplexity + accountComplexity + funcComplexity
          }

          _ <- foldScriptTransfers(blockchain, tx, dAppAddress)(ps, dataAndPaymentDiff)
        } yield {
          val paymentReceiversMap: Map[Address, Portfolio] = Monoid
            .combineAll(pmts)
            .mapValues(mp => mp.toList.map(x => Portfolio.build(Asset.fromCompatId(x._1), x._2)))
            .mapValues(l => Monoid.combineAll(l))
          val paymentFromContractMap = Map(dAppAddress -> Monoid.combineAll(paymentReceiversMap.values).negate)
          val transfers              = Monoid.combineAll(Seq(paymentReceiversMap, paymentFromContractMap))
          val isr = InvokeScriptResult(data = dataEntries, transfers = paymentReceiversMap.toVector.flatMap {
            case (addr, pf) => InvokeScriptResult.paymentsFromPortfolio(addr, pf)
          })
          val dataAndPaymentDiffTx = dataAndPaymentDiff.transactions(tx.id())
          val dataAndPaymentDiffTxWithTransfers = dataAndPaymentDiffTx.copy(_2 = dataAndPaymentDiffTx._2 ++ transfers.keys)
          val transferSetDiff = Diff.stateOps(portfolios = transfers, scriptResults = Map(tx.id() -> isr))
          dataAndPaymentDiff.copy(
            transactions = dataAndPaymentDiff.transactions.updated(tx.id(), dataAndPaymentDiffTxWithTransfers),
            scriptsRun = scriptsInvoked + 1,
            scriptsComplexity = scriptsComplexity
          ) |+| transferSetDiff
        }
      case Left(l) => TracedResult(Left(l))
      case _       => TracedResult(Left(GenericError(s"No contract at address ${tx.dAppAddressOrAlias}")))
    }
  }

  private def payableAndDataPart(height: Int,
                                 tx: InvokeScriptTransaction,
                                 dAppAddress: Address,
                                 dataEntries: List[DataEntry[_]],
                                 feePart: Map[Address, Portfolio]) = {
    if (dataEntries.length > ContractLimits.MaxWriteSetSize) {
      Left(GenericError(s"WriteSet can't contain more than ${ContractLimits.MaxWriteSetSize} entries"))
    } else if (dataEntries.exists(_.key.getBytes("UTF-8").length > ContractLimits.MaxKeySizeInBytes)) {
      Left(GenericError(s"Key size must be less than ${ContractLimits.MaxKeySizeInBytes}"))
    } else {
      val totalDataBytes = dataEntries.map(_.toBytes.length).sum

      val payablePart: Map[Address, Portfolio] = tx.payment
        .map {
          case InvokeScriptTransaction.Payment(amt, assetId) =>
            assetId match {
              case asset @ IssuedAsset(_) =>
                Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amt))).combine(
                  Map(dAppAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> amt)))
                )
              case Waves =>
                Map(tx.sender.toAddress -> Portfolio(-amt, LeaseBalance.empty, Map.empty))
                  .combine(Map(dAppAddress -> Portfolio(amt, LeaseBalance.empty, Map.empty)))
            }
        }
        .foldLeft(Map[Address, Portfolio]())(_ combine _)

      if (totalDataBytes <= ContractLimits.MaxWriteSetSizeInBytes) {
        Right(
          Diff(tx = tx,
               portfolios = feePart combine payablePart,
               accountData = Map(dAppAddress -> AccountDataInfo(dataEntries.map(d => d.key -> d).toMap)))
        )
      } else
        Left(GenericError(s"WriteSet size can't exceed ${ContractLimits.MaxWriteSetSizeInBytes} bytes, actual: $totalDataBytes bytes"))
    }
  }

  private def foldScriptTransfers(blockchain: Blockchain, tx: InvokeScriptTransaction, dAppAddress: Address)(
      ps: List[(Recipient.Address, Long, Option[ByteStr])],
      dataDiff: Diff): TracedResult[ValidationError, Diff] = {
    if (ps.length <= ContractLimits.MaxPaymentAmount) {
      val foldResult = ps.foldLeft(TracedResult(dataDiff.asRight[ValidationError])) { (tracedDiffAcc, payment) =>
        val (addressRepr, amount, asset) = payment
        val address                      = Address.fromBytes(addressRepr.bytes.arr).explicitGet()
        val tracedDiff: TracedResult[ValidationError, Diff] = Asset.fromCompatId(asset) match {
          case Waves =>
            Diff
              .stateOps(
                portfolios = Map(
                  address     -> Portfolio(amount, LeaseBalance.empty, Map.empty),
                  dAppAddress -> Portfolio(-amount, LeaseBalance.empty, Map.empty)
                )
              )
              .asRight[ValidationError]
          case a @ IssuedAsset(id) =>
            val nextDiff = Diff.stateOps(
              portfolios = Map(
                address     -> Portfolio(0, LeaseBalance.empty, Map(a -> amount)),
                dAppAddress -> Portfolio(0, LeaseBalance.empty, Map(a -> -amount))
              ))
            blockchain.assetScript(a) match {
              case None =>
                nextDiff.asRight[ValidationError]
              case Some(script) =>
                val assetValidationDiff = tracedDiffAcc.resultE.flatMap(
                  d => validateScriptTransferWithSmartAssetScript(blockchain, tx)(d, addressRepr, amount, asset, nextDiff, script)
                )
                val errorOpt = assetValidationDiff.fold(Some(_), _ => None)
                TracedResult(
                  assetValidationDiff,
                  List(AssetVerifierTrace(id, errorOpt))
                )
            }
        }
        tracedDiffAcc |+| tracedDiff
      }
      TracedResult(foldResult.resultE.map(_ => Diff.stateOps()), foldResult.trace)
    } else {
      Left(GenericError(s"Too many ScriptTransfers: max: ${ContractLimits.MaxPaymentAmount}, actual: ${ps.length}"))
    }
  }

  private def validateScriptTransferWithSmartAssetScript(blockchain: Blockchain, tx: InvokeScriptTransaction)(
      totalDiff: Diff,
      addressRepr: Recipient.Address,
      amount: Long,
      asset: Option[ByteStr],
      nextDiff: Diff,
      script: Script): Either[ValidationError, Diff] = {
    Try {
      ScriptRunner(
        Coproduct[TxOrd](
          ScriptTransfer(
            asset,
            Recipient.Address(tx.dAppAddressOrAlias.bytes),
            Recipient.Address(addressRepr.bytes),
            amount,
            tx.timestamp,
            tx.id()
          )),
        CompositeBlockchain(blockchain, Some(totalDiff)),
        script,
        isAssetScript = true,
        tx.dAppAddressOrAlias.bytes
      ) match {
        case (log, Left(error))  => Left(ScriptExecutionError(error, log, isAssetScript = true))
        case (log, Right(FALSE)) => Left(TransactionNotAllowedByScript(log, isAssetScript = true))
        case (_, Right(TRUE))    => Right(nextDiff)
        case (log, Right(x))     => Left(ScriptExecutionError(s"Script returned not a boolean result, but $x", log, isAssetScript = true))
      }
    } match {
      case Failure(e) =>
        Left(ScriptExecutionError(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, isAssetScript = true))
      case Success(s) => s
    }
  }
}

package com.wavesplatform.state.diffs

import cats.implicits._
import cats.kernel.Monoid
import com.google.common.base.Throwables
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, LogItem, ScriptResult}
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient}
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
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
import com.wavesplatform.metrics._
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.{Failure, Success, Try}

object InvokeScriptTransactionDiff {

  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(blockchain: Blockchain, height: Int)(tx: InvokeScriptTransaction): TracedResult[ValidationError, Diff] = {
    val accScript = blockchain.accountScript(tx.dappAddress)

    def evalContract(contract: DApp): Either[ScriptExecutionError, ScriptResult] = {
      val environment = new WavesEnvironment(
        AddressScheme.current.chainId,
        Coeval(tx.asInstanceOf[In]),
        Coeval(height),
        blockchain,
        Coeval(tx.dappAddress.bytes)
      )
      val invoker                                       = tx.sender.toAddress.bytes
      val maybePayment: Option[(Long, Option[ByteStr])] = tx.payment.headOption.map(p => (p.amount, p.assetId.compatId))
      val invocation                                    = ContractEvaluator.Invocation(
        tx.funcCallOpt,
        Recipient.Address(invoker),
        tx.sender,
        maybePayment,
        tx.dappAddress.bytes,
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
                PureContext.build(V3),
                CryptoContext.build(Global),
                WavesContext.build(directives, environment)
              )
            )
            .evaluationContext,
          contract,
          invocation
        )
      } yield evaluator

      result.leftMap { case (error, log) => ScriptExecutionError(error, log, isAssetScript = false) }
    }

    accScript match {
      case Some(ContractScriptImpl(_, contract, _)) =>
        val functionName = tx.funcCallOpt.map(_.function.asInstanceOf[FunctionHeader.User].name).getOrElse(ContractEvaluator.DEFAULT_FUNC_NAME)

        val contractFunc =
          if (tx.funcCallOpt.nonEmpty)
            contract.callableFuncs.find(_.u.name == functionName).flatMap(_ => tx.funcCallOpt)
          else
            contract.defaultFuncOpt.map(defFunc => FUNCTION_CALL(FunctionHeader.User(defFunc.u.name), List.empty))

        contractFunc match {
          case None => Left(GenericError(s"No function '$functionName' at address ${tx.dappAddress}"))
          case Some(funcCall) =>
            val scriptResultE =
              stats.invokedScriptExecution.measureForType(InvokeScriptTransaction.typeId)(evalContract(contract))
            for {
              scriptResult <- TracedResult(scriptResultE, List(InvokeScriptTrace(tx.dappAddress, Some(funcCall), scriptResultE)))
              ScriptResult(ds, ps) = scriptResult

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
              wavesFee = feeInfo._1
              dataAndPaymentDiff <- TracedResult(payableAndDataPart(height, tx, ds, feeInfo._2))
              _                  <- TracedResult(Either.cond(pmts.flatMap(_.values).flatMap(_.values).forall(_ >= 0), (), NegativeAmount(-42, "")))
              _ <- TracedResult(
                validateOverflow(pmts.flatMap(_.values).flatMap(_.values), "Attempt to transfer unavailable funds in contract payment"))
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
              _ <- foldScriptTransfers(blockchain, tx)(ps, dataAndPaymentDiff)
            } yield {
              val paymentReceiversMap: Map[Address, Portfolio] = Monoid
                .combineAll(pmts)
                .mapValues(mp => mp.toList.map(x => Portfolio.build(Asset.fromCompatId(x._1), x._2)))
                .mapValues(l => Monoid.combineAll(l))
              val paymentFromContractMap = Map(tx.dappAddress -> Monoid.combineAll(paymentReceiversMap.values).negate)
              val transfers = Monoid.combineAll(Seq(paymentReceiversMap, paymentFromContractMap))
              dataAndPaymentDiff.copy(scriptsRun = scriptsInvoked + 1) |+| Diff.stateOps(portfolios = transfers)
            }
        }
      case _ => Left(GenericError(s"No contract at address ${tx.dappAddress}"))
    }
  }

  private def payableAndDataPart(height: Int, tx: InvokeScriptTransaction, ds: List[DataItem[_]], feePart: Map[Address, Portfolio]) = {
    val dataEntries: Seq[DataEntry[_]] = ds.map {
      case DataItem.Bool(k, b) => BooleanDataEntry(k, b)
      case DataItem.Str(k, b)  => StringDataEntry(k, b)
      case DataItem.Lng(k, b)  => IntegerDataEntry(k, b)
      case DataItem.Bin(k, b)  => BinaryDataEntry(k, b)
    }
    if (dataEntries.length > ContractLimits.MaxWriteSetSize) {
      Left(GenericError(s"WriteSec can't contain more than ${ContractLimits.MaxWriteSetSize} entries"))
    } else if (dataEntries.exists(_.key.getBytes().length > ContractLimits.MaxKeySizeInBytes)) {
      Left(GenericError(s"Key size must be less than ${ContractLimits.MaxKeySizeInBytes}"))
    } else {
      val totalDataBytes = dataEntries.map(_.toBytes.length).sum

      val payablePart: Map[Address, Portfolio] = tx.payment
        .map {
          case InvokeScriptTransaction.Payment(amt, assetId) =>
            assetId match {
              case asset @ IssuedAsset(_) =>
                Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amt))).combine(
                  Map(tx.dappAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> amt)))
                )
              case Waves =>
                Map(tx.sender.toAddress -> Portfolio(-amt, LeaseBalance.empty, Map.empty))
                  .combine(Map(tx.dappAddress -> Portfolio(amt, LeaseBalance.empty, Map.empty)))
            }
        }
        .foldLeft(Map[Address, Portfolio]())(_ combine _)

      if (totalDataBytes <= ContractLimits.MaxWriteSetSizeInBytes) {
        val recordedData = InvokeScriptResult(
          dataEntries,
          payablePart.toVector.flatMap {
            case (addr, portfolio) =>
              val waves  = InvokeScriptResult.Payment(addr, Waves, portfolio.balance)
              val assets = portfolio.assets.map { case (assetId, amount) => InvokeScriptResult.Payment(addr, assetId, amount) }
              (assets ++ Some(waves)).filter(_.amount != 0)
          }
        )

        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = feePart combine payablePart,
            accountData = Map(tx.dappAddress -> AccountDataInfo(dataEntries.map(d => d.key -> d).toMap)),
            scriptResults = Map(tx.id()      -> recordedData)
          )
        )
      } else
        Left(GenericError(s"WriteSet size can't exceed ${ContractLimits.MaxWriteSetSizeInBytes} bytes, actual: $totalDataBytes bytes"))
    }
  }

  private def foldScriptTransfers(blockchain: Blockchain, tx: InvokeScriptTransaction)(ps: List[(Recipient.Address, Long, Option[ByteStr])],
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
                  address        -> Portfolio(amount, LeaseBalance.empty, Map.empty),
                  tx.dappAddress -> Portfolio(-amount, LeaseBalance.empty, Map.empty)
                )
              )
              .asRight[ValidationError]
          case a @ IssuedAsset(id) =>
            val nextDiff = Diff.stateOps(
              portfolios = Map(
                address        -> Portfolio(0, LeaseBalance.empty, Map(a -> amount)),
                tx.dappAddress -> Portfolio(0, LeaseBalance.empty, Map(a -> -amount))
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
      stats.assetScriptExecution.measureForType(InvokeScriptTransaction.typeId)(
        ScriptRunner(
          blockchain.height,
          Coproduct[TxOrd](
            ScriptTransfer(
              asset,
              Recipient.Address(tx.dappAddress.bytes),
              Recipient.Address(addressRepr.bytes),
              amount,
              tx.timestamp,
              tx.id()
          )),
          CompositeBlockchain.composite(blockchain, totalDiff),
          script,
          isAssetScript = true,
          tx.dappAddress.bytes
        )
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

package com.wavesplatform.transaction.smart

import cats.Id
import cats.syntax.either.*
import cats.syntax.functor.*
import com.google.common.base.Throwables
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TermPrinter
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.metrics.*
import com.wavesplatform.state.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction.assets.exchange.{EthOrders, ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.AssetVerifierTrace.AssetContext
import com.wavesplatform.transaction.smart.script.trace.{AccountVerifierTrace, AssetVerifierTrace, TraceStep, TracedResult}
import com.wavesplatform.utils.ScorexLogging
import org.msgpack.core.annotations.VisibleForTesting
import shapeless.Coproduct

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Verifier extends ScorexLogging {

  private val stats = TxProcessingStats

  import stats.TxTimerExt

  type ValidationResult[T] = Either[ValidationError, T]

  def apply(blockchain: Blockchain, limitedExecution: Boolean = false, enableExecutionLog: Boolean = false)(
      tx: Transaction
  ): TracedResult[ValidationError, Int] = (tx: @unchecked) match {
    case _: GenesisTransaction  => Right(0)
    case _: EthereumTransaction => Right(0)
    case pt: ProvenTransaction =>
      (pt, blockchain.accountScript(pt.sender.toAddress)) match {
        case (stx: PaymentTransaction, None) =>
          stats.signatureVerification
            .measureForType(stx.tpe)(stx.firstProofIsValidSignatureBeforeV6)
            .as(0)
        case (et: ExchangeTransaction, scriptOpt) =>
          verifyExchange(
            et,
            blockchain,
            scriptOpt,
            if (limitedExecution) ContractLimits.FailFreeInvokeComplexity else Int.MaxValue,
            enableExecutionLog
          )
        case (tx: SigProofsSwitch, Some(_)) if tx.usesLegacySignature =>
          Left(GenericError("Can't process transaction with signature from scripted account"))
        case (_: PaymentTransaction, Some(_)) =>
          Left(GenericError("Can't process transaction with signature from scripted account"))
        case (_: InvokeExpressionTransaction, Some(script)) if forbidInvokeExpressionDueToVerifier(script.script) =>
          Left(
            GenericError(s"Can't process InvokeExpressionTransaction from RIDE ${script.script.stdLibVersion} verifier, it might be used from $V6")
          )
        case (_, Some(script)) =>
          stats.accountScriptExecution
            .measureForType(pt.tpe)(
              verifyTx(blockchain, script.script, script.verifierComplexity.toInt, pt, None, enableExecutionLog)
            )
        case _ =>
          stats.signatureVerification
            .measureForType(tx.tpe)(verifyAsEllipticCurveSignature(pt, blockchain.isFeatureActivated(BlockchainFeatures.RideV6)))
            .as(0)
      }
  }

  private def forbidInvokeExpressionDueToVerifier(s: Script): Boolean =
    s match {
      case e: ExprScript if e.stdLibVersion < V6                                             => true
      case c: ContractScriptImpl if c.stdLibVersion < V6 && c.expr.verifierFuncOpt.isDefined => true
      case _                                                                                 => false
    }

  /** Verifies asset scripts and returns snapshot with complexity. In case of error returns spent complexity */
  def assets(blockchain: Blockchain, remainingComplexity: Int, enableExecutionLog: Boolean)(
      tx: TransactionBase
  ): TracedResult[(Long, ValidationError), StateSnapshot] = {
    case class AssetForCheck(asset: IssuedAsset, script: AssetScriptInfo, assetType: AssetContext)

    @tailrec
    def loop(
        assets: List[AssetForCheck],
        fullComplexity: Long,
        fullTrace: List[TraceStep],
        fullAttributes: TracedResult.Attributes
    ): (Long, TracedResult[ValidationError, Int]) = {
      assets match {
        case AssetForCheck(asset, AssetScriptInfo(script, estimatedComplexity), context) :: remaining =>
          val complexityLimit =
            if (remainingComplexity == Int.MaxValue) remainingComplexity
            else remainingComplexity - fullComplexity.toInt

          def verify = verifyTx(
            blockchain,
            script,
            estimatedComplexity.toInt,
            tx,
            Some(asset.id),
            enableExecutionLog,
            complexityLimit,
            context
          )

          stats.assetScriptExecution.measureForType(tx.tpe)(verify) match {
            case TracedResult(e @ Left(_), trace, attributes) =>
              (fullComplexity + estimatedComplexity, TracedResult(e, fullTrace ::: trace, fullAttributes ++ attributes))
            case TracedResult(Right(complexity), trace, attributes) =>
              loop(remaining, fullComplexity + complexity, fullTrace ::: trace, fullAttributes ++ attributes)
          }
        case Nil => (fullComplexity, TracedResult(Right(0), fullTrace))
      }
    }

    def assetScript(asset: IssuedAsset): Option[AssetScriptInfo] =
      blockchain.assetDescription(asset).flatMap(_.script)

    val assets = for {
      asset  <- tx.smartAssets(blockchain).toList
      script <- assetScript(asset)
    } yield AssetForCheck(asset, script, AssetContext.fromTxAndAsset(tx, asset))

    val additionalAssets = tx match {
      case e: ExchangeTransaction =>
        for {
          asset  <- List(e.buyOrder.matcherFeeAssetId, e.sellOrder.matcherFeeAssetId).distinct.collect { case ia: IssuedAsset => ia }
          script <- assetScript(asset)
        } yield AssetForCheck(asset, script, AssetContext.MatcherFee)

      case _ => Nil
    }

    val (complexity, result)  = loop(assets, 0L, Nil, Map.empty)
    val (_, additionalResult) = loop(additionalAssets, 0L, Nil, Map.empty)

    result
      .flatMap(_ => additionalResult)
      .leftMap(ve => (complexity, ve))
      .map(_ => StateSnapshot(scriptsComplexity = complexity))
  }

  private def logIfNecessary(
      result: Either[ValidationError, ?],
      id: String,
      execLog: Log[Id],
      execResult: Either[String, EVALUATED]
  ): Unit =
    result match {
      case Left(_) if log.logger.isDebugEnabled => log.debug(buildLogs(id, execLog, execResult))
      case _ if log.logger.isTraceEnabled       => log.trace(buildLogs(id, execLog, execResult))
      case _                                    => ()
    }

  private def verifyTx(
      blockchain: Blockchain,
      script: Script,
      estimatedComplexity: Int,
      transaction: TransactionBase,
      assetIdOpt: Option[ByteStr],
      enableExecutionLog: Boolean,
      complexityLimit: Int = Int.MaxValue,
      assetContext: AssetContext.Value = AssetContext.Unknown
  ): TracedResult[ValidationError, Int] = {

    val isAsset       = assetIdOpt.nonEmpty
    val senderAddress = transaction.asInstanceOf[Authorized].sender.toAddress

    val resultE = Try {
      val containerAddress = assetIdOpt.fold(Coproduct[Environment.Tthis](Recipient.Address(ByteStr(senderAddress.bytes))))(v =>
        Coproduct[Environment.Tthis](Environment.AssetId(v.arr))
      )
      val (log, evaluatedComplexity, result) =
        ScriptRunner(
          Coproduct[TxOrd](transaction),
          blockchain,
          script,
          isAsset,
          containerAddress,
          enableExecutionLog,
          complexityLimit
        )
      val complexity = if (blockchain.storeEvaluatedComplexity) evaluatedComplexity else estimatedComplexity
      val resultE = result match {
        case Left(execError) => Left(ScriptExecutionError(execError.message, log, assetIdOpt))
        case Right(FALSE)    => Left(TransactionNotAllowedByScript(log, assetIdOpt))
        case Right(TRUE)     => Right(complexity)
        case Right(x)        => Left(ScriptExecutionError(s"Script returned not a boolean result, but $x", log, assetIdOpt))
      }
      val logId = s"transaction ${transaction.id()}"
      logIfNecessary(resultE, logId, log, result.leftMap(_.message))
      resultE
    } match {
      case Failure(e) =>
        Left(ScriptExecutionError(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, assetIdOpt))
      case Success(s) => s
    }

    val createTrace = { (maybeError: Option[ValidationError]) =>
      val trace = assetIdOpt match {
        case Some(assetId) => AssetVerifierTrace(assetId, maybeError, assetContext)
        case None          => AccountVerifierTrace(senderAddress, maybeError)
      }
      List(trace)
    }

    resultE match {
      case Right(_)    => TracedResult(resultE, createTrace(None))
      case Left(error) => TracedResult(resultE, createTrace(Some(error)))
    }
  }

  private def verifyOrder(
      blockchain: Blockchain,
      script: AccountScriptInfo,
      order: Order,
      complexityLimit: Int,
      enableExecutionLog: Boolean
  ): ValidationResult[Int] =
    Try(
      ScriptRunner(
        Coproduct[ScriptRunner.TxOrd](order),
        blockchain,
        script.script,
        isAssetScript = false,
        Coproduct[Environment.Tthis](Recipient.Address(ByteStr(order.sender.toAddress.bytes))),
        enableExecutionLog,
        complexityLimit
      )
    ).toEither
      .leftMap(e => ScriptExecutionError(s"Uncaught execution error: $e", Nil, None))
      .flatMap { case (log, evaluatedComplexity, evaluationResult) =>
        val complexity = if (blockchain.storeEvaluatedComplexity) evaluatedComplexity else script.verifierComplexity.toInt
        val verifierResult = evaluationResult match {
          case Left(execError) => Left(ScriptExecutionError(execError.message, log, None))
          case Right(FALSE)    => Left(TransactionNotAllowedByScript(log, None))
          case Right(TRUE)     => Right(complexity)
          case Right(x)        => Left(GenericError(s"Script returned not a boolean result, but $x"))
        }
        val logId = s"order ${order.idStr()}"
        logIfNecessary(verifierResult, logId, log, evaluationResult.leftMap(_.message))
        verifierResult
      }

  private def verifyExchange(
      et: ExchangeTransaction,
      blockchain: Blockchain,
      matcherScriptOpt: Option[AccountScriptInfo],
      complexityLimit: Int,
      enableExecutionLog: Boolean
  ): TracedResult[ValidationError, Int] = {

    val typeId    = et.tpe
    val sellOrder = et.sellOrder
    val buyOrder  = et.buyOrder

    def matcherTxVerification: TracedResult[ValidationError, Int] =
      matcherScriptOpt
        .map { script =>
          if (et.version != 1) {
            stats.accountScriptExecution
              .measureForType(typeId)(
                verifyTx(
                  blockchain,
                  script.script,
                  script.verifierComplexity.toInt,
                  et,
                  None,
                  enableExecutionLog,
                  complexityLimit
                )
              )
          } else {
            TracedResult(Left(GenericError("Can't process transaction with signature from scripted account")))
          }
        }
        .getOrElse(
          stats.signatureVerification
            .measureForType(typeId)(verifyAsEllipticCurveSignature(et, blockchain.isFeatureActivated(BlockchainFeatures.RideV6)).as(0))
        )

    def orderVerification(order: Order): TracedResult[ValidationError, Int] = {
      val verificationResult = blockchain
        .accountScript(order.sender.toAddress)
        .map { asi =>
          if (order.version != 1) {
            stats.orderValidation.withoutTags().measure(verifyOrder(blockchain, asi, order, complexityLimit, enableExecutionLog))
          } else {
            Left(GenericError("Can't process order with signature from scripted account"))
          }
        }
        .getOrElse(
          stats.signatureVerification
            .measureForType(typeId)(
              verifyOrderSignature(
                order,
                blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
              ).as(0)
            )
        )

      TracedResult(verificationResult)
    }

    for {
      matcherComplexity <- matcherTxVerification
      sellerComplexity  <- orderVerification(sellOrder)
      buyerComplexity   <- orderVerification(buyOrder)
    } yield matcherComplexity + sellerComplexity + buyerComplexity
  }

  def verifyOrderSignature(order: Order, isRideV6Activated: Boolean): Either[GenericError, Order] =
    order.eip712Signature match {
      case Some(ethSignature) =>
        val signerKey = EthOrders.recoverEthSignerKey(order, ethSignature.arr)
        Either.cond(signerKey == order.senderPublicKey, order, GenericError(s"Ethereum signature invalid for $order"))

      case _ => verifyAsEllipticCurveSignature(order, isRideV6Activated)
    }

  def verifyAsEllipticCurveSignature[T <: Proven](pt: T, isRideV6Activated: Boolean): Either[GenericError, T] =
    (if (isRideV6Activated) {
       pt.firstProofIsValidSignatureAfterV6
     } else {
       pt.firstProofIsValidSignatureBeforeV6
     }).map(_ => pt)

  @VisibleForTesting
  private[smart] def buildLogs(
      id: String,
      execLog: Log[Id],
      execResult: Either[String, EVALUATED]
  ): String = {
    val builder = new StringBuilder(s"Script for $id evaluated to $execResult")
    execLog
      .foldLeft(builder) {
        case (sb, (k, Right(v))) =>
          sb.append(s"\nEvaluated `$k` to ")
          v match {
            case obj: EVALUATED => TermPrinter().print(str => sb.append(str), obj); sb
            case a              => sb.append(a.toString)
          }
        case (sb, (k, Left(err))) => sb.append(s"\nFailed to evaluate `$k`: $err")
      }
      .toString
  }
}

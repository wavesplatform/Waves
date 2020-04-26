package com.wavesplatform.state.diffs.invoke

import cats.kernel.Monoid
import cats.syntax.either._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.FunctionCallPolicyProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.ContractCompiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, IncompleteResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.metrics._
import com.wavesplatform.settings.Constants
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.{Failure, Right, Success, Try}

object InvokeScriptTransactionDiff {

  private val stats = TxProcessingStats
  import stats.TxTimerExt

  def apply(blockchain: Blockchain, blockTime: Long, skipExecution: Boolean = false)(
      tx: InvokeScriptTransaction
  ): TracedResult[ValidationError, Diff] = {

    val dAppAddressEi = blockchain.resolveAlias(tx.dAppAddressOrAlias)
    val accScriptEi   = dAppAddressEi.map(blockchain.accountScript)
    val functionCall  = tx.funcCall

    accScriptEi match {
      case Right(Some(AccountScriptInfo(pk, ContractScriptImpl(version, contract), _, callableComplexities))) =>
        for {
          _           <- TracedResult.wrapE(checkCall(functionCall, blockchain).leftMap(GenericError.apply))
          dAppAddress <- TracedResult(dAppAddressEi)

          feeInfo <- TracedResult(InvokeDiffsCommon.calcFee(blockchain, tx))

          directives <- TracedResult.wrapE(DirectiveSet(version, Account, DAppType).leftMap(GenericError.apply))
          payments   <- TracedResult.wrapE(AttachedPaymentExtractor.extractPayments(tx, version, blockchain, DApp).leftMap(GenericError.apply))
          input      <- TracedResult.wrapE(buildThisValue(Coproduct[TxOrd](tx: Transaction), blockchain, directives, None).leftMap(GenericError.apply))

          invocationComplexity <- TracedResult {
            InvokeDiffsCommon.getInvocationComplexity(blockchain, tx, callableComplexities, dAppAddress)
          }

          _ <- TracedResult {
            val stepLimit = ContractLimits.MaxComplexityByVersion(version)
            val stepsNumber =
              if (invocationComplexity % stepLimit == 0)
                invocationComplexity / stepLimit
              else
                invocationComplexity / stepLimit + 1

            val minFee    = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit * stepsNumber
            val assetName = tx.assetFee._1.fold("WAVES")(_.id.toString)
            val txName    = Constants.TransactionNames(InvokeScriptTransaction.typeId)
            val stepsInfo = if (stepsNumber > 1) s" with $stepsNumber invocation steps" else ""
            Either.cond(
              feeInfo._1 >= minFee,
              (),
              GenericError(
                s"Fee in $assetName for $txName (${tx.assetFee._2} in $assetName)" +
                  s"$stepsInfo does not exceed minimal value of $minFee WAVES."
              )
            )
          }

          verifierComplexity = blockchain.accountScript(tx.sender).map(_.verifierComplexity).getOrElse(0L)

          result <- if (!skipExecution) {
            for {
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
                    evaluator <- Try {
                      ContractEvaluator.applyV2(
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
                    } match {
                      case Success(r) => r
                      case Failure(e) => Left((e.getMessage, List.empty))
                    }
                  } yield evaluator

                  result.leftMap { case (error, log) => ScriptExecutionError.dApp(error, log) }
                })
                TracedResult(
                  scriptResultE,
                  List(InvokeScriptTrace(tx.dAppAddressOrAlias, functionCall, scriptResultE.map(_._1), scriptResultE.fold(_.log, _._2)))
                )
              }

              doProcessActions = InvokeDiffsCommon.processActions(
                _,
                version,
                dAppAddress,
                pk,
                feeInfo,
                invocationComplexity,
                verifierComplexity,
                tx,
                blockchain,
                blockTime
              )

              resultDiff <- scriptResult._1 match {
                case ScriptResultV3(dataItems, transfers) => doProcessActions(dataItems ::: transfers)
                case ScriptResultV4(actions)              => doProcessActions(actions)
                case ir: IncompleteResult =>
                  TracedResult.wrapValue[Diff, ValidationError](Diff(tx = tx, continuationStates = Map(tx.id.value -> ir.expr)))
              }
            } yield resultDiff
          } else TracedResult.wrapValue(InvokeDiffsCommon.paymentsPart(tx, dAppAddress, feeInfo._2))
        } yield result

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

}

package com.wavesplatform.transaction.smart

import cats.syntax.either.*
import cats.syntax.flatMap.*
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.MultiPaymentPolicyProvider.*
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.miniev.State
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.lang.v1.evaluator.ctx.impl.unit
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.*
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, ExtendedInternalFunction}
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.{CommonError, ExecutionError}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.invoke.InvokeScript
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.*
import com.wavesplatform.transaction.smart.InvokeFunction.extractPayments

class InvokeFunction(reentrant: Boolean, delegate: BaseFunction) extends ExtendedInternalFunction(delegate) {
  override def toString: String = s"InvokeFunction($reentrant,$delegate)"

  override def buildExpression(state: State, args: List[EVALUATED]): Either[ExecutionError, EXPR] = state match {
    case ds: DAppState =>
      val invocationExpression: Either[ExecutionError, (EXPR, (InvokeScript, PublicKey, StdLibVersion))] = args match {
        case CaseObj(t, fields) :: function :: ARR(invokeArgs) :: ARR(payments) :: Nil =>
          val dappAddress = t match {
            case `addressType` =>
              fields
                .get("bytes")
                .collect { case bs: CONST_BYTESTR => bs.bs.arr }
                .fold(CommonError(s"object ${args.head} has no 'bytes' field").asLeft[Address]) { bs =>
                  Address.fromBytes(bs).leftMap(ia => CommonError(ia.reason))
                }
            case `aliasType` =>
              fields
                .get("alias")
                .collect { case al: CONST_STRING => al.s }
                .fold(CommonError(s"object ${args.head} contains no 'alias' field").asLeft[Address]) { aliasName =>
                  Alias
                    .create(aliasName)
                    .flatMap(alias => ds.blockchain().resolveAlias(alias))
                    .leftMap(ve => CommonError("Could not resolve alias", Some(ve)))
                }
            case _ => Left(CommonError(s"object ${args.head} is neither Address nor Alias"))
          }

          for {
            da <- dappAddress
            functionName <- function match {
              case fn: CONST_STRING => Right(fn.s)
              case `unit` => Right("default")
              case other => Left(CommonError(s"${other} is not a valid function name"))
            }
            (ver, dp, dAppPK) <- ds
              .blockchain()
              .accountScript(da)
              .toRight(CommonError(s"No contract at address $da"))
              .flatMap[CommonError, (StdLibVersion, DApp, PublicKey)](asi =>
                asi.script match {
                  case ContractScriptImpl(stdLibVersion, expr) => (stdLibVersion, expr, asi.publicKey).asRight
                  case _ => CommonError(s"Trying to call dApp on the account with expression script ($da)").asLeft
                }
              )
            cf <- dp.callableFuncs
              .find(_.u.name == functionName)
              .toRight(CommonError(s"Cannot find callable function `$functionName`"))
            _ <- Either.cond(
              cf.u.args.length == invokeArgs.size,
              cf,
              CommonError(s"Callable function '$functionName takes ${cf.u.args.length} args but ${invokeArgs.length} were(was) given")
            )
            paymentArgs <- extractPayments(payments, ds.blockchain())
            extracted <- AttachedPaymentExtractor
              .extractPayments(paymentArgs, ver, ds.blockchain().allowsMultiPayment, InvokerScript)
              .leftMap(e => CommonError(e))
          } yield {
            val funcCall = FUNCTION_CALL(FunctionHeader.User(functionName), invokeArgs.toList)
            val argsWithInvocation = (cf.annotation.invocationArgName :: cf.u.args)
              .zip(
                Bindings.buildInvocation(
                  ContractEvaluator.Invocation(
                    funcCall,
                    Recipient.Address(ByteStr(ds.currentDApp.toAddress.bytes)),
                    ds.currentDApp,
                    Recipient.Address(ByteStr(ds.root.sender.toAddress.bytes)),
                    ds.root.sender,
                    extracted,
                    ds.root.id(),
                    ds.root.fee,
                    ds.root.feeAssetId.compatId
                  ),
                  ver
                ) :: invokeArgs.toList
              )
              .map { case (argName, argValue) => LET(argName, argValue) }

            (dp.decs ++ argsWithInvocation).foldRight(cf.u.body) { case (decl, expr) =>
              BLOCK(decl, expr)
            } -> (InvokeScript(ds.currentDApp, da, funcCall, paymentArgs, ds.root), dAppPK, ver)
          }

        case _ =>
          CommonError("unsupported arguments").asLeft
      }
      invocationExpression
        .flatTap { case (_, (invokeScript, pk, ver)) =>
          ds.spendComplexity(delegate.costByLibVersion(ver)).flatMap(_ => ds.invoke(invokeScript, pk, reentrant, ver))
        }
        .map(_._1)
    case _ => Left(CommonError("Unsupported evaluator state"))
  }
}

object InvokeFunction {
  def extractPayments(payments: Seq[EVALUATED], blockchain: Blockchain): Either[CommonError, Seq[InvokeScriptTransaction.Payment]] =
    payments.foldLeft(Seq.empty[InvokeScriptTransaction.Payment].asRight[CommonError]) {
      case (Right(ps), po) =>
        po match {
          case p @ CaseObj(`paymentType`, fields) =>
            for {
              asset <- fields
                .get("assetId")
                .toRight(CommonError(s"Payment $p has no 'assetId' field"))
                .flatMap {
                  case `unit` => (Waves: Asset).asRight[CommonError]
                  case CONST_BYTESTR(assetId) =>
                    val asset = IssuedAsset(assetId)
                    blockchain
                      .assetDescription(asset)
                      .fold(CommonError(s"asset $assetId is not found on the blockchain").asLeft[IssuedAsset]) { _ => asset.asRight[CommonError] }
                  case other => Left(CommonError(s"$other is not a valid asset"))
                }
              amt <- fields
                .get("amount")
                .toRight(CommonError(s"Payment $p has no 'amount' field"))
                .flatMap {
                  case CONST_LONG(v) if v >= 0 =>
                    v.asRight[CommonError]
                  case other => CommonError(s"$other is not a valid amount").asLeft[Long]
                }
            } yield ps :+ InvokeScriptTransaction.Payment(amt, asset)
          case other => Left(CommonError(s"Unexpected payment argument: $other"))
        }
      case (l @ Left(_), _) => l
    }
}

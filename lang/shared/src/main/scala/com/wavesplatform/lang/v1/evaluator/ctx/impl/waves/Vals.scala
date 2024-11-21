package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Eval, Monad}
import com.wavesplatform.lang.{CommonError, ExecutionError}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.{buildAssetInfo, ordType, orderObject, transactionObject}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.*
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{OrdType, Ord, Tx, PseudoTx, Recipient}
import com.wavesplatform.lang.v1.traits.domain.Tx.{BurnPseudoTx, InvokePseudoTx, ReissuePseudoTx, ScriptTransfer, SponsorFeePseudoTx}

object Vals {
  def tx(
      isTokenContext: Boolean,
      version: StdLibVersion,
      proofsEnabled: Boolean,
      fixBigScriptField: Boolean
  ): (String, (UNION, ContextfulVal[Environment])) =
    (GlobalValNames.Tx, (scriptInputType(isTokenContext, version, proofsEnabled), inputEntityVal(version, proofsEnabled, fixBigScriptField)))

  private def scriptInputType(isTokenContext: Boolean, version: StdLibVersion, proofsEnabled: Boolean) =
    if (isTokenContext)
      UNION(buildAssetSupportedTransactions(proofsEnabled, version))
    else
      UNION(buildOrderType(proofsEnabled, version) :: buildActiveTransactionTypes(proofsEnabled, version))

  private def inputEntityVal(version: StdLibVersion, proofsEnabled: Boolean, fixBigScriptField: Boolean): ContextfulVal[Environment] =
    new ContextfulVal.Lifted[Environment] {
      override def liftF[F[_]: Monad](env: Environment[F]): Eval[Either[ExecutionError, EVALUATED]] =
        Eval.later(
          env.inputEntity match {
            case tx: Tx => transactionObject(tx, proofsEnabled, version, fixBigScriptField).asRight[ExecutionError]
            case o: Ord => orderObject(o, proofsEnabled, version).asRight[ExecutionError]
            case ptx: PseudoTx =>
              ptx match {
                case b: BurnPseudoTx        => Bindings.mapBurnPseudoTx(b, version).asRight[ExecutionError]
                case r: ReissuePseudoTx     => Bindings.mapReissuePseudoTx(r, version).asRight[ExecutionError]
                case sf: SponsorFeePseudoTx => Bindings.mapSponsorFeePseudoTx(sf, version).asRight[ExecutionError]
                case st: ScriptTransfer     => Bindings.scriptTransfer(st, version).asRight[ExecutionError]
                case inv: InvokePseudoTx    => Bindings.mapInvokePseudoTx(inv, version).asRight[ExecutionError]
              }
          }
        )
    }

  val heightVal: ContextfulVal[Environment] =
    new ContextfulVal[Environment] {
      override def apply[F[_]: Monad](env: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        Eval.later {
          env.height
            .map(v => CONST_LONG(v): EVALUATED)
            .map(_.asRight[ExecutionError])
        }
    }

  val accountThisVal: ContextfulVal[Environment] =
    new ContextfulVal.Lifted[Environment] {
      override def liftF[F[_]: Monad](env: Environment[F]): Eval[Either[ExecutionError, EVALUATED]] =
        Eval.later {
          if (env.dAppAlias) {
            (FAIL("Use alias is disabled"): EVALUATED)
              .asRight[ExecutionError]
          } else {
            (Bindings.senderObject(
              env.tthis match {
                case a: Recipient.Address => a
                case _                    => throw new Exception("In the account's script value 'this` must be Address")
              }
            ): EVALUATED)
              .asRight[ExecutionError]
          }
        }
    }

  def assetThisVal(version: StdLibVersion): ContextfulVal[Environment] =
    new ContextfulVal[Environment] {
      override def apply[F[_]: Monad](env: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        Eval.later {
          env
            .assetInfoById(
              env.tthis match {
                case a: Recipient.Address     => throw new Exception("In the account's script value 'this` must be Address")
                case aid: Environment.AssetId => aid.id
              }
            )
            .map(v => buildAssetInfo(v.get, version): EVALUATED)
            .map(_.asRight[ExecutionError])
        }
    }

  def lastBlockVal(version: StdLibVersion): ContextfulVal[Environment] =
    new ContextfulVal[Environment] {
      override def apply[F[_]: Monad](env: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        Eval.later {
          env
            .lastBlockOpt()
            .map(v => Bindings.buildBlockInfo(v.get, version): EVALUATED)
            .map(_.asRight[ExecutionError])
        }
    }

  def lastBlock(version: StdLibVersion) = (GlobalValNames.LastBlock, (blockInfo(version), lastBlockVal(version)))

  val sellOrdTypeVal: ContextfulVal[Environment] = ContextfulVal.fromEval(Eval.now(Right(ordType(OrdType.Sell))))
  val buyOrdTypeVal: ContextfulVal[Environment]  = ContextfulVal.fromEval(Eval.now(Right(ordType(OrdType.Buy))))

  val sell = (GlobalValNames.Sell, (ordTypeType, sellOrdTypeVal))
  val buy  = (GlobalValNames.Buy, (ordTypeType, buyOrdTypeVal))

  val height: (String, (LONG.type, ContextfulVal[Environment])) = (GlobalValNames.Height, (LONG, heightVal))

  val accountThis: (String, (CASETYPEREF, ContextfulVal[Environment])) = (GlobalValNames.This, (addressType, accountThisVal))
  def assetThis(version: StdLibVersion): (String, (CASETYPEREF, ContextfulVal[Environment])) =
    (GlobalValNames.This, (assetType(version), assetThisVal(version)))

}

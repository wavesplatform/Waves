package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Eval, Monad}
import com.wavesplatform.lang.{CommonError, ExecutionError}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.{buildAssetInfo, ordType, orderObject, transactionObject}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.*
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.OrdType
import com.wavesplatform.lang.v1.traits.domain.Tx.{BurnPseudoTx, InvokePseudoTx, ReissuePseudoTx, ScriptTransfer, SponsorFeePseudoTx}

object Vals {
  def tx(
      isTokenContext: Boolean,
      version: StdLibVersion,
      proofsEnabled: Boolean,
      fixBigScriptField: Boolean
  ): (String, (UNION, ContextfulVal[Environment])) =
    (Tx, (scriptInputType(isTokenContext, version, proofsEnabled), inputEntityVal(version, proofsEnabled, fixBigScriptField)))

  private def scriptInputType(isTokenContext: Boolean, version: StdLibVersion, proofsEnabled: Boolean) =
    if (isTokenContext)
      UNION(buildAssetSupportedTransactions(proofsEnabled, version))
    else
      UNION(buildOrderType(proofsEnabled) :: buildActiveTransactionTypes(proofsEnabled, version))

  private def inputEntityVal(version: StdLibVersion, proofsEnabled: Boolean, fixBigScriptField: Boolean): ContextfulVal[Environment] =
    new ContextfulVal.Lifted[Environment] {
      override def liftF[F[_]: Monad](env: Environment[F]): Eval[Either[ExecutionError, EVALUATED]] =
        Eval.later(
          env.inputEntity
            .eliminate(
              tx => transactionObject(tx, proofsEnabled, version, fixBigScriptField).asRight[ExecutionError],
              _.eliminate(
                o => orderObject(o, proofsEnabled, version).asRight[ExecutionError],
                _.eliminate(
                  {
                    case b: BurnPseudoTx        => Bindings.mapBurnPseudoTx(b, version).asRight[ExecutionError]
                    case r: ReissuePseudoTx     => Bindings.mapReissuePseudoTx(r, version).asRight[ExecutionError]
                    case sf: SponsorFeePseudoTx => Bindings.mapSponsorFeePseudoTx(sf, version).asRight[ExecutionError]
                    case st: ScriptTransfer     => Bindings.scriptTransfer(st, version).asRight[ExecutionError]
                    case inv: InvokePseudoTx    => Bindings.mapInvokePseudoTx(inv, version).asRight[ExecutionError]
                  },
                  _ => CommonError("Expected Transaction or Order").asLeft[EVALUATED]
                )
              )
            )
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
              env.tthis.eliminate(identity, _ => throw new Exception("In the account's script value 'this` must be Address"))
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
              env.tthis.eliminate(
                _ => throw new Exception("In the account's script value 'this` must be Address"),
                _.eliminate(_.id, v => throw new Exception(s"Incorrect value $v for 'this'"))
              )
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

  def lastBlock(version: StdLibVersion) = (LastBlock, (blockInfo(version), lastBlockVal(version)))

  val sellOrdTypeVal: ContextfulVal[Environment] = ContextfulVal.fromEval(Eval.now(Right(ordType(OrdType.Sell))))
  val buyOrdTypeVal: ContextfulVal[Environment]  = ContextfulVal.fromEval(Eval.now(Right(ordType(OrdType.Buy))))

  val sell = (Sell, (ordTypeType, sellOrdTypeVal))
  val buy  = (Buy, (ordTypeType, buyOrdTypeVal))

  val height: (String, (LONG.type, ContextfulVal[Environment])) = (Height, (LONG, heightVal))

  val accountThis: (String, (CASETYPEREF, ContextfulVal[Environment])) = (This, (addressType, accountThisVal))
  def assetThis(version: StdLibVersion): (String, (CASETYPEREF, ContextfulVal[Environment])) =
    (This, (assetType(version), assetThisVal(version)))

}

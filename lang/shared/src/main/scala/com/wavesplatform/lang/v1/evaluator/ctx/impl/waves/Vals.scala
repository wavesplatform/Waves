package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.implicits._
import cats.{Eval, Monad}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EVALUATED}
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, LONG, UNION}
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.{buildAssetInfo, ordType, orderObject, transactionObject}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.OrdType

object Vals {
  def tx(
    isTokenContext: Boolean,
    version: StdLibVersion,
    proofsEnabled: Boolean
  ): (ExecutionError, (UNION, ContextfulVal[Environment])) =
    ("tx", (scriptInputType(isTokenContext, version, proofsEnabled), inputEntityVal(version, proofsEnabled)))

  private def scriptInputType(isTokenContext: Boolean, version: StdLibVersion, proofsEnabled: Boolean) =
    if (isTokenContext)
      UNION(buildAssetSupportedTransactions(proofsEnabled, version))
    else
      UNION(buildOrderType(proofsEnabled) :: buildActiveTransactionTypes(proofsEnabled, version))

  private def inputEntityVal(version: StdLibVersion, proofsEnabled: Boolean): ContextfulVal[Environment] =
    new ContextfulVal.Lifted[Environment] {
      override def liftF[F[_] : Monad](env: Environment[F]): Eval[Either[ExecutionError, EVALUATED]] =
        Eval.later(
          env.inputEntity
            .eliminate(
              tx => transactionObject(tx, proofsEnabled, version).asRight[ExecutionError],
              _.eliminate(
                o => orderObject(o, proofsEnabled, version).asRight[ExecutionError],
                _.eliminate(
                  o => Bindings.scriptTransfer(o).asRight[ExecutionError],
                  _ => "Expected Transaction or Order".asLeft[EVALUATED]
                )
              )
            )
        )
    }

  val heightVal: ContextfulVal[Environment] =
    new ContextfulVal[Environment] {
      override def apply[F[_] : Monad](env: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        Eval.later {
          env.height
            .map(v => CONST_LONG(v): EVALUATED)
            .map(_.asRight[ExecutionError])
        }
    }

  val accountThisVal: ContextfulVal[Environment] =
    new ContextfulVal.Lifted[Environment] {
      override def liftF[F[_] : Monad](env: Environment[F]): Eval[Either[ExecutionError, EVALUATED]] =
        Eval.later {
          (Bindings.senderObject(env.tthis): EVALUATED)
            .asRight[ExecutionError]
        }
    }

  val assetThisVal: ContextfulVal[Environment] =
    new ContextfulVal[Environment] {
      override def apply[F[_] : Monad](env: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        Eval.later {
          env.assetInfoById(env.tthis.bytes)
            .map(v => buildAssetInfo(v.get): EVALUATED)
            .map(_.asRight[ExecutionError])
        }
    }

  val lastBlockVal: ContextfulVal[Environment] =
    new ContextfulVal[Environment] {
      override def apply[F[_] : Monad](env: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        Eval.later {
          env.lastBlockOpt()
            .map(v => Bindings.buildLastBlockInfo(v.get): EVALUATED)
            .map(_.asRight[ExecutionError])
        }
    }

  val lastBlock = ("lastBlock", (blockInfo, lastBlockVal))

  val sellOrdTypeVal: ContextfulVal[Environment] = ContextfulVal.fromEval(Eval.now(Right(ordType(OrdType.Sell))))
  val buyOrdTypeVal:  ContextfulVal[Environment] = ContextfulVal.fromEval(Eval.now(Right(ordType(OrdType.Buy))))

  val sell = ("Sell", (ordTypeType, sellOrdTypeVal))
  val buy = ("Buy", (ordTypeType, buyOrdTypeVal))

  val height: (ExecutionError, (LONG.type, ContextfulVal[Environment])) = ("height", (LONG, heightVal))

  val accountThis: (ExecutionError, (CASETYPEREF, ContextfulVal[Environment])) = ("this", (addressType, accountThisVal))
  val assetThis: (ExecutionError, (CASETYPEREF, ContextfulVal[Environment]))   = ("this", (assetType,   assetThisVal))

}

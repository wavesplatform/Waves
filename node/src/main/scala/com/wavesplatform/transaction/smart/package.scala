package com.wavesplatform.transaction

import cats.syntax.either.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, Asset as AssetType, DApp as DAppType}
import com.wavesplatform.lang.v1.traits.domain.PseudoTx
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.traits.Environment.{InputEntity, Tthis, AssetId}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.{DApp as DAppTarget}

package object smart {
  def buildThisValue(
      in: TxOrd,
      blockchain: Blockchain,
      ds: DirectiveSet,
      scriptContainerAddress: Tthis
  ): Either[String, InputEntity] =
    in match {
      case tx: TransactionBase      => RealTransactionWrapper(tx, blockchain, ds.stdLibVersion, paymentTarget(ds, scriptContainerAddress))
      case order: Order             => RealTransactionWrapper.ord(order).asRight
      case scriptTransfer: PseudoTx => scriptTransfer.asRight
    }

  def paymentTarget(
      ds: DirectiveSet,
      scriptContainerAddress: Tthis
  ): AttachedPaymentTarget =
    (ds.scriptType, ds.contentType) match {
      case (Account, DAppType)   => DAppTarget
      case (Account, Expression) => InvokerScript
      case (AssetType, Expression) =>
        scriptContainerAddress match {
          case _: Recipient.Address => throw new Exception("Not a AssetId")
          case a: AssetId           => AssetScript(ByteStr(a.id))
        }
      case _ => ???
    }
}

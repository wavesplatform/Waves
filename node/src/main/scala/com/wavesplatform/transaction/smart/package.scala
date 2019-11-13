package com.wavesplatform.transaction

import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, Asset => AssetType, DApp => DAppType}
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.{DApp => DAppTarget}
import shapeless._

package object smart {
  def buildThisValue(
      in: TxOrd,
      blockchain: Blockchain,
      ds: DirectiveSet,
      scriptContainerAddress: Option[ByteStr]
  ): Either[ExecutionError, InputEntity] =
    in.eliminate(
      tx =>
        RealTransactionWrapper(tx, blockchain, ds.stdLibVersion, paymentTarget(ds, scriptContainerAddress))
          .map(Coproduct[InputEntity](_)),
      _.eliminate(
        order => Coproduct[InputEntity](RealTransactionWrapper.ord(order)).asRight[ExecutionError],
        _.eliminate(
          scriptTransfer => Coproduct[InputEntity](scriptTransfer).asRight[ExecutionError],
          _ => ???
        )
      )
    )

  def paymentTarget(
      ds: DirectiveSet,
      scriptContainerAddress: Option[ByteStr]
  ): AttachedPaymentTarget =
    (ds.scriptType, ds.contentType, scriptContainerAddress) match {
      case (Account, DAppType, _)                 => DAppTarget
      case (Account, Expression, _)               => InvokerScript
      case (AssetType, Expression, Some(assetId)) => AssetScript(assetId)
      case _                                      => ???
    }
}

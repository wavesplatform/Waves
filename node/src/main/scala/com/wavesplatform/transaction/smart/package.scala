package com.wavesplatform.transaction

import cats.implicits._
import com.wavesplatform.features.MultiPaymentPolicyProvider._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import shapeless._

package object smart {
  def mapInput(in: TxOrd, blockchain: Blockchain, ds: DirectiveSet): Either[ExecutionError, InputEntity] =
    in.eliminate(
      tx => RealTransactionWrapper(tx, blockchain.multiPaymentAllowed, ds).map(Coproduct[InputEntity](_)),
      _.eliminate(
        order => Coproduct[InputEntity](RealTransactionWrapper.ord(order)).asRight[ExecutionError],
        _.eliminate(
          scriptTransfer => Coproduct[InputEntity](scriptTransfer).asRight[ExecutionError],
          ???
        )
      )
    )
}

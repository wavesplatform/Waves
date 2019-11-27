package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object IssueTxValidator extends TxValidator[IssueTransaction] {
  override def validate(tx: IssueTransaction): ValidatedV[IssueTransaction] = {
    import tx._
    V.seq(tx)(
      V.positiveAmount(quantity, "assets"),
      V.assetName(name),
      V.assetDescription(description),
      V.assetDecimals(decimals),
      (if(version == TxVersion.Pseudo) {
        V.zeroFee(fee)
      } else {
       V.fee(fee)
      }),
      V.cond(version > TxVersion.V1 || script.isEmpty, GenericError("Script not supported")),
      V.cond(script.forall(_.isInstanceOf[ExprScript]), GenericError(s"Asset can only be assigned with Expression script, not Contract"))
    )
  }
}

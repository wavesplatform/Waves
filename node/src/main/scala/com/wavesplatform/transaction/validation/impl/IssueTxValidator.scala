package com.wavesplatform.transaction.validation.impl

import cats.data.Validated
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}
import com.wavesplatform.transaction.{TxValidationError, TxVersion}

object IssueTxValidator extends TxValidator[IssueTransaction] {
  override def validate(tx: IssueTransaction): ValidatedV[IssueTransaction] = {
    def assetDecimals(decimals: Byte): ValidatedV[Byte] = {
      Validated
        .condNel(
          decimals >= 0 && decimals <= IssueTransaction.MaxAssetDecimals,
          decimals,
          TxValidationError.TooBigArray
        )
    }

    import tx._
    V.seq(tx)(
      V.assetName(tx.name),
      V.assetDescription(tx.description),
      assetDecimals(decimals),
      V.cond(version > TxVersion.V1 || script.isEmpty, GenericError("Script not supported")),
      V.cond(script.forall(_.isInstanceOf[ExprScript]), GenericError(s"Asset can only be assigned with Expression script, not Contract"))
    )
  }
}

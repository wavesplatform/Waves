package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}
import com.wavesplatform.transaction.{Asset, TxValidationError}
import com.wavesplatform.utils.StringBytes

object UpdateAssetInfoTxValidator extends TxValidator[UpdateAssetInfoTransaction] {
  override def validate(tx: UpdateAssetInfoTransaction): ValidatedV[UpdateAssetInfoTransaction] =
    V.seq(tx)(
      V.cond(UpdateAssetInfoTransaction.supportedVersions(tx.version), TxValidationError.UnsupportedVersion(tx.version)),
      V.asset[IssuedAsset](tx.assetId),
      V.asset[Asset](tx.feeAsset),
      V.assetName(tx.name.toByteString),
      V.assetDescription(tx.description.toByteString)
    )
}

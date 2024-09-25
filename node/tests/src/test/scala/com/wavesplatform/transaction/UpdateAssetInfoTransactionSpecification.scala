package com.wavesplatform.transaction

import com.wavesplatform.TestValues
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.{InvalidName, TooBigArray}
import com.wavesplatform.transaction.assets.IssueTransaction.{MaxAssetDescriptionLength, MaxAssetNameLength, MinAssetNameLength}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction

class UpdateAssetInfoTransactionSpecification extends PropSpec {

  property("new asset name validation") {
    val invalidNames = Seq(
      "",
      "a" * (MinAssetNameLength - 1),
      "a" * (MaxAssetNameLength + 1),
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )
    val validNames = Seq("a" * MinAssetNameLength, "a" * MaxAssetNameLength)

    invalidNames.foreach { name =>
      createUpdateAssetInfoTx(name = name) shouldBe Left(InvalidName)
    }

    validNames.foreach { name =>
      createUpdateAssetInfoTx(name = name) should beRight
    }
  }

  property("new asset description validation") {
    val invalidDescs = Seq("a" * (MaxAssetDescriptionLength + 1))
    val validDescs   = Seq("", "a" * MaxAssetDescriptionLength)

    invalidDescs.foreach { desc =>
      createUpdateAssetInfoTx(description = desc) shouldBe Left(TooBigArray)
    }

    validDescs.foreach { desc =>
      createUpdateAssetInfoTx(description = desc) should beRight
    }
  }

  def createUpdateAssetInfoTx(
      name: String = "updated_name",
      description: String = "updated_description"
  ): Either[ValidationError, UpdateAssetInfoTransaction] =
    UpdateAssetInfoTransaction.create(
      version = TxVersion.V1,
      sender = TxHelpers.signer(1).publicKey,
      assetId = ByteStr.fill(DigestLength)(1),
      name = name,
      description = description,
      timestamp = System.currentTimeMillis(),
      feeAmount = TestValues.fee,
      feeAsset = Waves,
      proofs = Proofs.empty,
      chainId = AddressScheme.current.chainId
    )
}

package scorex.transaction.assets

import com.wavesplatform.crypto
import com.wavesplatform.state._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction._

object CancelFeeSponsorshipTransaction {

  def create(version: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, SponsorFeeTransaction] =
    if (!SponsorFeeTransaction.supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(SponsorFeeTransaction(version, sender, assetId, None, fee, timestamp, proofs))
    }

  def create(version: Byte, sender: PrivateKeyAccount, assetId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, SponsorFeeTransaction] =
    create(version, sender, assetId, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
}

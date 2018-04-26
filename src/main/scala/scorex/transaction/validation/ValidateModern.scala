package scorex.transaction.validation

import cats.implicits._
import com.wavesplatform.state.{ByteStr, DataEntry}
import scorex.account.{AddressOrAlias, PublicKeyAccount}
import scorex.transaction.assets.MassTransferTransaction.ParsedTransfer
import scorex.transaction.assets.{MassTransferTransaction, SmartIssueTransaction, VersionedTransferTransaction}
import scorex.transaction.modern.assets._
import scorex.transaction.modern.lease.{LeaseCancelPayload, LeasePayload}
import scorex.transaction.modern.{DataPayload, TxHeader}
import scorex.transaction.smart.script.Script
import scorex.transaction.{AssetId, Proofs}

object ValidateModern {

  def issueTx(version: Byte,
              chainId: Byte,
              sender: PublicKeyAccount,
              name: Array[Byte],
              description: Array[Byte],
              quantity: Long,
              decimals: Byte,
              reissuable: Boolean,
              script: Option[Script],
              fee: Long,
              timestamp: Long,
              proofs: Proofs): Validated[SmartIssueTransaction] = {
    (validateVersion(SmartIssueTransaction.supportedVersions, version),
     validateName(name),
     validateDescription(description),
     validateAmount(quantity, "assets"),
     validateDecimals(decimals),
     validateTimestamp(timestamp),
     validateFee(fee))
      .mapN {
        case (ver, n, desc, am, dec, ts, f) =>
          SmartIssueTransaction(ver, chainId, sender, n, desc, am, dec, reissuable, script, f, ts, proofs)
      }
  }

  def massTransferTx(version: Byte,
                     assetId: Option[AssetId],
                     sender: PublicKeyAccount,
                     transfers: List[ParsedTransfer],
                     timestamp: Long,
                     feeAmount: Long,
                     attachment: Array[Byte],
                     proofs: Proofs): Validated[MassTransferTransaction] = {
    (validateVersion(MassTransferTransaction.supportedVersions, version),
     validateTransfers(transfers),
     validateTimestamp(timestamp),
     validateFee(feeAmount),
     validateAttachment(attachment))
      .mapN {
        case (ver, trs, ts, fee, att) =>
          MassTransferTransaction(ver, assetId, sender, trs, ts, fee, att, proofs)
      }
  }

  def transferTx(version: Byte,
                 assetId: Option[AssetId],
                 sender: PublicKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 fee: Long,
                 attachment: Array[Byte],
                 proofs: Proofs): Validated[VersionedTransferTransaction] = {
    (validateVersion(VersionedTransferTransaction.supportedVersions, version),
     validateAmount(amount, "waves"),
     validateFee(fee),
     validateSum(List(amount, fee)),
     validateTimestamp(timestamp),
     validateAttachment(attachment))
      .mapN {
        case (ver, am, f, _, ts, att) =>
          VersionedTransferTransaction(ver, sender, recipient, assetId, am, f, ts, att, proofs)
      }
  }

  def header(supportedVersions: Set[Byte])(`type`: Byte, version: Byte, sender: PublicKeyAccount, fee: Long, timestamp: Long): Validated[TxHeader] = {
    (validateVersion(supportedVersions, version), validateFee(fee), validateTimestamp(timestamp))
      .mapN {
        case (ver, f, ts) => TxHeader(`type`, ver, sender, f, ts)
      }
  }

  def burnPL(assetId: ByteStr, amount: Long): Validated[BurnPayload] = {
    validateAmount(amount, assetId.base58)
      .map(am => BurnPayload(assetId, am))
  }

  def reissuePL(assetId: AssetId, quantity: Long, reissuable: Boolean): Validated[ReissuePayload] = {
    validateAmount(quantity, assetId.base58)
      .map(am => ReissuePayload(assetId, am, reissuable))
  }

  def issuePL(chainId: Byte,
              name: Array[Byte],
              description: Array[Byte],
              quantity: Long,
              decimals: Byte,
              reissuable: Boolean,
              script: Option[Script]): Validated[IssuePayload] = {
    (validateName(name), validateDescription(description), validateAmount(quantity, name.toString), validateDecimals(decimals))
      .mapN {
        case (n, d, a, dec) =>
          IssuePayload(chainId, n, d, a, dec, reissuable, script)
      }
  }

  def leasePL(amount: Long, recipient: AddressOrAlias): Validated[LeasePayload] = {
    validateAmount(amount, "waves")
      .map(am => LeasePayload(amount, recipient))
  }

  def leaseCancelPL(leaseId: ByteStr): Validated[LeaseCancelPayload] = {
    LeaseCancelPayload(leaseId).validNel
  }

  def transferPL(recipient: AddressOrAlias,
                 assetId: Option[AssetId],
                 feeAssetId: Option[AssetId],
                 amount: Long,
                 attachment: Array[Byte]): Validated[TransferPayload] = {
    val assetName = assetId.map(_.base58).getOrElse("waves")
    (validateAmount(amount, assetName), validateAttachment(attachment))
      .mapN {
        case (am, at) =>
          TransferPayload(recipient, assetId, feeAssetId, am, at)
      }
  }

  def dataPL(entries: List[DataEntry[_]]): Validated[DataPayload] = {
    validateDataEntries(entries)
      .map(es => DataPayload(es))
  }

  def sponsorFellPL(assetId: AssetId, minFee: Long): Validated[SponsorFeePayload] = {
    validateMinFee(minFee, assetId.base58)
      .map(f => SponsorFeePayload(assetId, f))
  }
}

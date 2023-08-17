package com.wavesplatform.state

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
import org.bouncycastle.crypto.digests.Blake2bDigest

import java.nio.charset.StandardCharsets
import scala.collection.mutable

object TxStateSnapshotHashBuilder {
  object KeyType extends Enumeration {
    val WavesBalance, AssetBalance, DataEntry, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship, Alias, VolumeAndFee, AssetStatic,
        AssetVolume, AssetNameDescription, TransactionStatus = Value
  }

  val InitStateHash: ByteStr = ByteStr(crypto.fastHash(""))

  final case class Result(txStateSnapshotHash: ByteStr) {
    def createHash(prevHash: ByteStr): ByteStr =
      TxStateSnapshotHashBuilder.createHash(Seq(prevHash, txStateSnapshotHash))
  }

  def createHashFromSnapshot(snapshot: StateSnapshot, txInfoOpt: Option[NewTransactionInfo]): Result = {
    val changedKeys = mutable.Map.empty[ByteStr, Array[Byte]]

    def addEntry(keyType: KeyType.Value, key: Array[Byte]*)(value: Array[Byte]*): Unit = {
      val solidKey   = ByteStr(key.fold(Array(keyType.id.toByte))(_ ++ _))
      val solidValue = value.foldLeft(Array.emptyByteArray)(_ ++ _)
      changedKeys(solidKey) = solidValue
    }

    snapshot.balances.foreach { case ((address, asset), balance) =>
      asset match {
        case Waves              => addEntry(KeyType.WavesBalance, address.bytes)(Longs.toByteArray(balance))
        case asset: IssuedAsset => addEntry(KeyType.AssetBalance, address.bytes, asset.id.arr)(Longs.toByteArray(balance))
      }
    }

    snapshot.leaseBalances.foreach { case (address, balance) =>
      addEntry(KeyType.LeaseBalance, address.bytes)(Longs.toByteArray(balance.in), Longs.toByteArray(balance.out))
    }

    for {
      (address, data) <- snapshot.accountData
      entry           <- data.values
    } addEntry(KeyType.DataEntry, address.bytes, entry.key.getBytes(StandardCharsets.UTF_8))(entry.valueBytes)

    snapshot.aliases.foreach { case (alias, address) =>
      addEntry(KeyType.Alias, address.bytes, alias.name.getBytes(StandardCharsets.UTF_8))()
    }

    snapshot.accountScriptsByAddress.foreach { case (address, sv) =>
      addEntry(KeyType.AccountScript, address.bytes)(sv.fold(Array.emptyByteArray)(_.script.bytes().arr))
    }

    for {
      (asset, sv) <- snapshot.assetScripts
      script = sv.map(_.script)
    } addEntry(KeyType.AssetScript, asset.id.arr)(script.fold(Array.emptyByteArray)(_.bytes().arr))

    snapshot.leaseStates.foreach { case (leaseId, details) =>
      addEntry(KeyType.LeaseStatus, leaseId.arr)(booleanToBytes(details.isActive))
    }

    snapshot.sponsorships.foreach { case (asset, sponsorship) =>
      addEntry(KeyType.Sponsorship, asset.id.arr)(Longs.toByteArray(sponsorship.minFee))
    }

    snapshot.orderFills.foreach { case (orderId, fillInfo) =>
      addEntry(KeyType.VolumeAndFee, orderId.arr)(
        Longs.toByteArray(fillInfo.volume),
        Longs.toByteArray(fillInfo.fee)
      )
    }

    snapshot.assetStatics.foreach { case (asset, assetInfo) =>
      addEntry(KeyType.AssetStatic, asset.id.arr)(
        assetInfo.issuerPublicKey.toByteArray,
        Array(assetInfo.decimals.toByte),
        booleanToBytes(assetInfo.nft)
      )
    }

    snapshot.assetVolumes.foreach { case (asset, volume) =>
      addEntry(KeyType.AssetVolume, asset.id.arr)(
        booleanToBytes(volume.isReissuable),
        volume.volume.toByteArray
      )
    }

    snapshot.assetNamesAndDescriptions.foreach { case (asset, assetInfo) =>
      addEntry(KeyType.AssetNameDescription, asset.id.arr)(
        assetInfo.name.toByteArray,
        assetInfo.description.toByteArray,
        Ints.toByteArray(assetInfo.lastUpdatedAt.toInt)
      )
    }

    txInfoOpt.foreach(txInfo =>
      txInfo.status match {
        case Status.Failed    => addEntry(KeyType.TransactionStatus, txInfo.transaction.id().arr)(Array(1: Byte))
        case Status.Elided    => addEntry(KeyType.TransactionStatus, txInfo.transaction.id().arr)(Array(2: Byte))
        case Status.Succeeded =>
      }
    )

    Result(createHash(changedKeys.toSeq.sortBy(_._1).flatMap { case (k, v) => Seq(k, ByteStr(v)) }))
  }

  def createGenesisStateHash(txs: Seq[GenesisTransaction]): ByteStr =
    ByteStr(
      txs
        .foldLeft(InitStateHash.arr -> Map.empty[Address, Long]) { case ((prevStateHash, balances), tx) =>
          val newBalance = balances.getOrElse(tx.recipient, 0L) + tx.amount.value
          val tsh =
            crypto.fastHash(
              Array(TxStateSnapshotHashBuilder.KeyType.WavesBalance.id.toByte) ++ tx.recipient.bytes ++ Longs.toByteArray(newBalance)
            )
          val newStateHash = crypto.fastHash(prevStateHash ++ tsh)
          newStateHash -> balances.updated(tx.recipient, newBalance)
        }
        ._1
    )

  private def booleanToBytes(flag: Boolean): Array[Byte] =
    if (flag) Array(1: Byte) else Array(0: Byte)

  private def createHash(bs: Iterable[ByteStr], digestFn: Blake2bDigest = newDigestInstance()): ByteStr = {
    bs.foreach(bs => digestFn.update(bs.arr, 0, bs.arr.length))
    val result = new Array[Byte](crypto.DigestLength)
    digestFn.doFinal(result, 0)
    ByteStr(result)
  }

  private def newDigestInstance(): Blake2bDigest = new Blake2bDigest(crypto.DigestLength * 8)
}

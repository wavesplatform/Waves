package com.wavesplatform.state

import cats.data.Ior

import java.nio.charset.StandardCharsets
import cats.syntax.monoid.*
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import org.bouncycastle.crypto.digests.Blake2bDigest

import scala.collection.mutable

object TxStateSnapshotHashBuilder {
  object KeyType extends Enumeration {
    val WavesBalance, AssetBalance, DataEntry, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship, Alias, VolumeAndFee,
        StaticAssetInfo, AssetReissuability, AssetNameDescription = Value
  }

  val InitStateHash: ByteStr = ByteStr(crypto.fastHash(""))

  final case class Result(txStateSnapshotHash: ByteStr) {
    def createHash(prevHash: ByteStr): ByteStr =
      TxStateSnapshotHashBuilder.createHash(Seq(prevHash, txStateSnapshotHash))
  }

  def createHashFromTxDiff(blockchain: Blockchain, txDiff: Diff): Result = {
    val changedKeys = mutable.Map.empty[ByteStr, Array[Byte]]

    def addEntry(keyType: KeyType.Value, key: Array[Byte]*)(value: Array[Byte]*): Unit = {
      val solidKey   = ByteStr(key.fold(Array(keyType.id.toByte))(_ ++ _))
      val solidValue = value.foldLeft(Array.emptyByteArray)(_ ++ _)
      changedKeys(solidKey) = solidValue
    }

    val PortfolioUpdates(updatedBalances, updatedLeaseBalances) = DiffToStateApplier.portfolios(blockchain, txDiff)

    for {
      (address, assets) <- updatedBalances
      (asset, balance)  <- assets
    } asset match {
      case Waves              => addEntry(KeyType.WavesBalance, address.bytes)(Longs.toByteArray(balance))
      case asset: IssuedAsset => addEntry(KeyType.AssetBalance, address.bytes, asset.id.arr)(Longs.toByteArray(balance))
    }

    updatedLeaseBalances foreach { case (address, balance) =>
      addEntry(KeyType.LeaseBalance, address.bytes)(Longs.toByteArray(balance.in), Longs.toByteArray(balance.out))
    }

    for {
      (address, data) <- txDiff.accountData
      entry           <- data.values
    } addEntry(KeyType.DataEntry, address.bytes, entry.key.getBytes(StandardCharsets.UTF_8))(entry.valueBytes)

    txDiff.aliases.foreach { case (alias, address) =>
      addEntry(KeyType.Alias, address.bytes, alias.name.getBytes(StandardCharsets.UTF_8))()
    }

    txDiff.scripts.foreach { case (address, sv) =>
      addEntry(KeyType.AccountScript, address.bytes)(sv.fold(Array.emptyByteArray)(_.script.bytes().arr))
    }

    for {
      (asset, sv) <- txDiff.assetScripts
      script = sv.map(_.script)
    } addEntry(KeyType.AssetScript, asset.id.arr)(script.fold(Array.emptyByteArray)(_.bytes().arr))

    txDiff.leaseState.foreach { case (leaseId, details) =>
      addEntry(KeyType.LeaseStatus, leaseId.arr)(booleanToBytes(details.isActive))
    }

    txDiff.sponsorship.foreach { case (asset, sponsorship) =>
      val minSponsoredFee =
        sponsorship match {
          case SponsorshipValue(minFee) => minFee
          case SponsorshipNoInfo        => 0L
        }

      addEntry(KeyType.Sponsorship, asset.id.arr)(Longs.toByteArray(minSponsoredFee))
    }

    txDiff.orderFills.foreach { case (orderId, fillInfo) =>
      val filledVolumeAndFee = blockchain.filledVolumeAndFee(orderId).combine(fillInfo)
      addEntry(KeyType.VolumeAndFee, orderId.arr)(Longs.toByteArray(filledVolumeAndFee.volume), Longs.toByteArray(filledVolumeAndFee.fee))
    }

    txDiff.issuedAssets.foreach { case (asset, assetInfo) =>
      addEntry(KeyType.StaticAssetInfo, asset.id.arr)(
        assetInfo.static.issuer.toAddress.bytes,
        Array(assetInfo.static.decimals.toByte),
        booleanToBytes(assetInfo.static.nft)
      )
    }

    val assetReissuabilities = txDiff.issuedAssets.map { case (asset, assetInfo) =>
      asset -> assetInfo.volume.isReissuable
    } ++ txDiff.updatedAssets.collect {
      case (asset, Ior.Right(volume))   => asset -> volume.isReissuable
      case (asset, Ior.Both(_, volume)) => asset -> volume.isReissuable
    }

    assetReissuabilities.foreach { case (asset, isReissuable) =>
      addEntry(KeyType.AssetReissuability, asset.id.arr)(booleanToBytes(isReissuable))
    }

    val assetNameDescs = txDiff.issuedAssets.map { case (asset, assetInfo) =>
      (asset, (assetInfo.dynamic.name.toByteArray, assetInfo.dynamic.description.toByteArray, assetInfo.dynamic.lastUpdatedAt.toInt))
    } ++ txDiff.updatedAssets.collect {
      case (asset, Ior.Left(assetInfo)) =>
        (asset, (assetInfo.name.toByteArray, assetInfo.description.toByteArray, assetInfo.lastUpdatedAt.toInt))
      case (asset, Ior.Both(assetInfo, _)) =>
        (asset, (assetInfo.name.toByteArray, assetInfo.description.toByteArray, assetInfo.lastUpdatedAt.toInt))
    }

    assetNameDescs.foreach { case (asset, (name, description, changeHeight)) =>
      addEntry(KeyType.AssetNameDescription, asset.id.arr)(name, description, Ints.toByteArray(changeHeight))
    }

    Result(createHash(changedKeys.toSeq.sortBy(_._1).flatMap { case (k, v) => Seq(k, ByteStr(v)) }))
  }

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

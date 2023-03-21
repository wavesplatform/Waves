package com.wavesplatform.state

import cats.data.Ior

import java.nio.charset.StandardCharsets
import cats.syntax.monoid.*
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.TxStateSnapshotHashBuilder.{KeyType, Result}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import org.bouncycastle.crypto.digests.Blake2bDigest

import scala.collection.mutable

class TxStateSnapshotHashBuilder {
  private[this] val changedKeys = mutable.TreeMap.empty[ByteStr, Array[Byte]]

  def createHashFromTxDiff(blockchain: Blockchain, txDiff: Diff): Result = {
    val PortfolioUpdates(updatedBalances, updatedLeaseBalances) = DiffToStateApplier.portfolios(blockchain, txDiff)

    for {
      (address, assets) <- updatedBalances
      (asset, balance)  <- assets
    } asset match {
      case Waves              => addWavesBalance(address, balance)
      case asset: IssuedAsset => addAssetBalance(address, asset, balance)
    }

    updatedLeaseBalances foreach { case (address, balance) =>
      addLeaseBalance(address, balance.in, balance.out)
    }

    for {
      (address, data) <- txDiff.accountData
      entry           <- data.data.values
    } addDataEntry(address, entry)

    txDiff.aliases.foreach { case (alias, address) =>
      addAlias(address, alias.name)
    }

    txDiff.scripts.foreach { case (address, sv) =>
      addAccountScript(address, sv.map(_.script))
    }

    for {
      (address, sv) <- txDiff.assetScripts
      script = sv.map(_.script)
    } addAssetScript(address, script)

    txDiff.leaseState.foreach { case (leaseId, details) =>
      addLeaseStatus(TransactionId @@ leaseId, details.isActive)
    }

    txDiff.sponsorship.foreach { case (asset, sponsorship) =>
      addSponsorship(
        asset,
        sponsorship match {
          case SponsorshipValue(minFee) => minFee
          case SponsorshipNoInfo        => 0L
        }
      )
    }

    txDiff.orderFills.foreach { case (orderId, fillInfo) =>
      val filledVolumeAndFee = blockchain.filledVolumeAndFee(orderId).combine(fillInfo)
      addVolumeAndFee(orderId, filledVolumeAndFee.volume, filledVolumeAndFee.fee)
    }

    txDiff.issuedAssets.foreach { case (asset, assetInfo) =>
      addStaticAssetInfo(asset, assetInfo.static.issuer.toAddress, assetInfo.static.nft)
    }

    val assetReissuabilities = txDiff.issuedAssets.map { case (asset, assetInfo) =>
      asset -> assetInfo.volume.isReissuable
    }.toSeq ++ txDiff.updatedAssets.collect {
      case (asset, Ior.Right(volume))   => asset -> volume.isReissuable
      case (asset, Ior.Both(_, volume)) => asset -> volume.isReissuable
    }

    assetReissuabilities.foreach { case (asset, isReissuable) => addAssetReissuability(asset, isReissuable) }

    val assetNameDescs = txDiff.issuedAssets.map { case (asset, assetInfo) =>
      (asset, assetInfo.dynamic.name.toByteArray, assetInfo.dynamic.description.toByteArray, assetInfo.dynamic.lastUpdatedAt.toInt)
    }.toSeq ++ txDiff.updatedAssets.collect { case (asset, Ior.Left(assetInfo)) =>
      (asset, assetInfo.name.toByteArray, assetInfo.description.toByteArray, assetInfo.lastUpdatedAt.toInt)
    }

    assetNameDescs.foreach { case (asset, name, description, changeHeight) => addAssetNameDescription(asset, name, description, changeHeight) }

    result()
  }

  private def addWavesBalance(address: Address, balance: Long): Unit = {
    addEntry(KeyType.WavesBalance, address.bytes)(Longs.toByteArray(balance))
  }

  private def addAssetBalance(address: Address, asset: IssuedAsset, balance: Long): Unit = {
    addEntry(KeyType.AssetBalance, address.bytes, asset.id.arr)(
      Longs.toByteArray(balance)
    )
  }

  private def addDataEntry(address: Address, dataEntry: DataEntry[?]): Unit = {
    addEntry(KeyType.DataEntry, address.bytes, dataEntry.key.getBytes(StandardCharsets.UTF_8))(
      dataEntry.valueBytes
    )
  }

  private def addAccountScript(address: Address, script: Option[Script]): Unit = {
    addEntry(KeyType.AccountScript, address.bytes)(
      script.fold(Array.emptyByteArray)(_.bytes().arr)
    )
  }

  private def addAssetScript(asset: IssuedAsset, script: Option[Script]): Unit = {
    addEntry(KeyType.AssetScript, asset.id.arr)(
      script.fold(Array.emptyByteArray)(_.bytes().arr)
    )
  }

  private def addLeaseBalance(address: Address, leaseIn: Long, leaseOut: Long): Unit = {
    addEntry(KeyType.LeaseBalance, address.bytes)(
      Longs.toByteArray(leaseIn),
      Longs.toByteArray(leaseOut)
    )
  }

  private def addLeaseStatus(leaseId: TransactionId, status: Boolean): Unit = {
    addEntry(KeyType.LeaseStatus, leaseId.arr)(
      booleanToBytes(status)
    )
  }

  private def addSponsorship(asset: IssuedAsset, minSponsoredFee: Long): Unit = {
    addEntry(KeyType.Sponsorship, asset.id.arr)(
      Longs.toByteArray(minSponsoredFee)
    )
  }

  private def addAlias(address: Address, alias: String): Unit = {
    addEntry(KeyType.Alias, address.bytes, alias.getBytes(StandardCharsets.UTF_8))()
  }

  private def addVolumeAndFee(orderId: ByteStr, filledVolume: Long, filledFee: Long): Unit = {
    addEntry(KeyType.VolumeAndFee, orderId.arr)(
      Longs.toByteArray(filledVolume),
      Longs.toByteArray(filledFee)
    )
  }

  private def addStaticAssetInfo(asset: IssuedAsset, issuer: Address, isNft: Boolean): Unit = {
    addEntry(KeyType.StaticAssetInfo, asset.id.arr)(
      issuer.bytes,
      booleanToBytes(isNft)
    )
  }

  private def addAssetReissuability(asset: IssuedAsset, isReissuable: Boolean): Unit = {
    addEntry(KeyType.AssetReissuability, asset.id.arr)(
      booleanToBytes(isReissuable)
    )
  }

  private def addAssetNameDescription(asset: IssuedAsset, name: Array[Byte], description: Array[Byte], changeHeight: Int): Unit = {
    addEntry(KeyType.AssetNameDescription, asset.id.arr)(
      name,
      description,
      Ints.toByteArray(changeHeight)
    )
  }

  private def result(): Result =
    Result(TxStateSnapshotHashBuilder.createHash(changedKeys.toSeq.sortBy(_._1).flatMap { case (k, v) => Seq(k, ByteStr(v)) }))

  private[this] def addEntry(keyType: KeyType.Value, key: Array[Byte]*)(value: Array[Byte]*): Unit = {
    val solidKey   = ByteStr(key.fold(Ints.toByteArray(keyType.id))(_ ++ _))
    val solidValue = value.foldLeft(Array.emptyByteArray)(_ ++ _)
    changedKeys(solidKey) = solidValue
  }

  private def booleanToBytes(flag: Boolean): Array[Byte] =
    if (flag) Array(1: Byte) else Array(0: Byte)
}

object TxStateSnapshotHashBuilder {
  object KeyType extends Enumeration {
    val WavesBalance, AssetBalance, DataEntry, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship, Alias, VolumeAndFee,
        StaticAssetInfo, AssetReissuability, AssetNameDescription = Value
  }

  final case class Result(txStateSnapshotHash: ByteStr) {
    def createHash(prevHash: ByteStr): ByteStr =
      TxStateSnapshotHashBuilder.createHash(Seq(prevHash, txStateSnapshotHash))
  }

  val EmptyHash: ByteStr = createHash(Seq(ByteStr("".getBytes(StandardCharsets.UTF_8))))

  private def createHash(bs: Iterable[ByteStr], digestFn: Blake2bDigest = newDigestInstance()): ByteStr = {
    bs.foreach(bs => digestFn.update(bs.arr, 0, bs.arr.length))
    val result = new Array[Byte](crypto.DigestLength)
    digestFn.doFinal(result, 0)
    ByteStr(result)
  }

  private def newDigestInstance(): Blake2bDigest = new Blake2bDigest(crypto.DigestLength * 8)
}

package com.wavesplatform.state

import cats.data.Ior

import java.nio.charset.StandardCharsets
import cats.syntax.monoid.*
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.GenesisTransaction
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

  def createHashFromDiff(blockchain: Blockchain, diff: Diff): Result = {
    val changedKeys = mutable.Map.empty[ByteStr, Array[Byte]]

    def addEntry(keyType: KeyType.Value, key: Array[Byte]*)(value: Array[Byte]*): Unit = {
      val solidKey   = ByteStr(key.fold(Array(keyType.id.toByte))(_ ++ _))
      val solidValue = value.foldLeft(Array.emptyByteArray)(_ ++ _)
      changedKeys(solidKey) = solidValue
    }

    val PortfolioUpdates(updatedBalances, updatedLeaseBalances) = DiffToStateApplier.portfolios(blockchain, diff)

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
      (address, data) <- diff.accountData
      entry           <- data.values
    } addEntry(KeyType.DataEntry, address.bytes, entry.key.getBytes(StandardCharsets.UTF_8))(entry.valueBytes)

    diff.aliases.foreach { case (alias, address) =>
      addEntry(KeyType.Alias, address.bytes, alias.name.getBytes(StandardCharsets.UTF_8))()
    }

    diff.scripts.foreach { case (address, sv) =>
      addEntry(KeyType.AccountScript, address.bytes)(
        sv.fold(Seq(Array.emptyByteArray))(scriptInfo =>
          Seq(scriptInfo.script.bytes().arr, scriptInfo.publicKey.arr, Longs.toByteArray(scriptInfo.verifierComplexity))
        )*
      )
    }

    for {
      (asset, sv) <- diff.assetScripts
      script = sv.map(_.script)
    } addEntry(KeyType.AssetScript, asset.id.arr)(script.fold(Array.emptyByteArray)(_.bytes().arr))

    diff.leaseState.foreach { case (leaseId, details) =>
      addEntry(KeyType.LeaseStatus, leaseId.arr)(booleanToBytes(details.isActive))
    }

    diff.sponsorship.foreach { case (asset, sponsorship) =>
      val minSponsoredFee =
        sponsorship match {
          case SponsorshipValue(minFee) => minFee
          case SponsorshipNoInfo        => 0L
        }

      addEntry(KeyType.Sponsorship, asset.id.arr)(Longs.toByteArray(minSponsoredFee))
    }

    diff.orderFills.foreach { case (orderId, fillInfo) =>
      val filledVolumeAndFee = blockchain.filledVolumeAndFee(orderId).combine(fillInfo)
      addEntry(KeyType.VolumeAndFee, orderId.arr)(Longs.toByteArray(filledVolumeAndFee.volume), Longs.toByteArray(filledVolumeAndFee.fee))
    }

    diff.issuedAssets.foreach { case (asset, assetInfo) =>
      addEntry(KeyType.StaticAssetInfo, asset.id.arr)(
        assetInfo.static.issuer.toAddress.bytes,
        Array(assetInfo.static.decimals.toByte),
        booleanToBytes(assetInfo.static.nft)
      )
    }

    val assetReissuabilities = diff.issuedAssets.map { case (asset, assetInfo) =>
      asset -> assetInfo.volume
    } ++ diff.updatedAssets.collect {
      case (asset, Ior.Right(volume))   => asset -> volume
      case (asset, Ior.Both(_, volume)) => asset -> volume
    }

    assetReissuabilities.foreach { case (asset, volume) =>
      addEntry(KeyType.AssetReissuability, asset.id.arr)(booleanToBytes(volume.isReissuable), volume.volume.toByteArray)
    }

    val assetNameDescs = diff.issuedAssets.map { case (asset, assetInfo) =>
      (asset, (assetInfo.dynamic.name.toByteArray, assetInfo.dynamic.description.toByteArray, assetInfo.dynamic.lastUpdatedAt.toInt))
    } ++ diff.updatedAssets.collect {
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

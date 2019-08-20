package com.wavesplatform.database

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.Distributions
import com.wavesplatform.state.{AddressId, AssetDistribution, AssetDistributionPage, Height, Portfolio, TxNum}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.utils.Paged
import monix.reactive.Observable

private[database] final class LevelDBDistributions(ldb: LevelDBWriter) extends Distributions {
  import ldb._

  def portfolio(a: Address): Portfolio =
    portfolioCache.get(a, () => loadPortfolio(a))

  def nftObservable(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] = readOnly { case db =>
    val assetIdStream = db
      .get(Keys.addressId(address))
      .map(assetBalanceIterator(db, _))
      .getOrElse(Nil)
      .map { case ((asset, _), _) => asset }
      .distinct
      .reverse

    val issueTxStream = assetIdStream
      .flatMap(ia => transactionInfo(ia.id).map(_._2))
      .collect {
        case itx: IssueTransaction if itx.isNFT => itx
      }

   val result = from
      .flatMap(ia => transactionInfo(ia.id))
      .fold(issueTxStream) {
        case (_, afterTx) =>
          issueTxStream
            .dropWhile(_.id() != afterTx.id())
            .drop(1)
      }

    Observable.fromIterable(result)
  }

  override def assetDistribution(asset: IssuedAsset): AssetDistribution = readOnly { db =>
    val (issueH, issueN) = getAssetHN(asset)

    val dst = (for {
      addressId <- db
        .iterateToSeq(Bytes.concat(Shorts.toByteArray(Keys.AddressesForAssetPrefix), Keys.heightWithNum(issueH, issueN)))(e => AddressId.fromBytes(e.getKey.takeRight(4)))
      balance <- loadBalanceForAssetHN(db)(addressId, issueH, issueN) if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    AssetDistribution(dst)
  }

  override def assetDistributionAtHeight(asset: IssuedAsset,
                                         height: Int,
                                         count: Int,
                                         fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage] = readOnly { db =>
    lazy val (issueH, issueN) = getAssetHN(asset)

    lazy val page: AssetDistributionPage = {
      val addressIds: Seq[AddressId] = {
        def takeAfter(s: Seq[AddressId], a: Option[AddressId]): Seq[AddressId] = {
          a match {
            case None    => s
            case Some(v) => s.dropWhile(_ != v).drop(1)
          }
        }

        val all = db
          .iterateToSeq(Bytes.concat(Shorts.toByteArray(Keys.AddressesForAssetPrefix), Keys.heightWithNum(issueH, issueN)))(e => AddressId.fromBytes(e.getKey.takeRight(AddressId.Bytes)))

        val maybeAddressId = fromAddress.flatMap(addr => db.get(Keys.addressId(addr)))
        takeAfter(all, maybeAddressId)
      }

      val distribution: Seq[(Address, Long)] =
        for {
          addressId <- addressIds.toStream
          balance <- db
            .lastValue(Keys.AssetBalancePrefix, Bytes.concat(AddressId.toBytes(addressId), Keys.heightWithNum(issueH, issueN)), height)
            .map(e => Longs.fromByteArray(e.getValue))
            .filter(_ > 0)
        } yield db.get(Keys.idToAddress(addressId)) -> balance

      val dst = distribution.take(count + 1).toStream

      val hasNext = dst.length > count
      val items   = if (hasNext) dst.init else dst
      val lastKey = items.lastOption.map(_._1)

      val result: Paged[Address, AssetDistribution] =
        Paged(hasNext, lastKey, AssetDistribution(items.toMap))
      AssetDistributionPage(result)
    }

    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)
    Either
      .cond(
        height > canGetAfterHeight,
        page,
        GenericError(s"Cannot get asset distribution at height less than ${canGetAfterHeight + 1}")
      )
  }

  private[this] def getAssetHNOption(asset: IssuedAsset): Option[(Height, TxNum)] = {
    assetHNCache.get(asset)
  }

  private[this] def getAssetHN(asset: IssuedAsset): (Height, TxNum) = {
    getAssetHNOption(asset).getOrElse((Height @@ 0, TxNum @@ 0.toShort))
  }

  override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    def createMap() = {
      val balances = db.iterateToSeq(Shorts.toByteArray(Keys.WavesBalancePrefix)) { e =>
        val (_, addressId, _, height) = Keys.parseAddressBytesHeight(e.getKey)
        (addressId, height) -> Longs.fromByteArray(e.getValue)
      }

      val byAddressId = balances.foldLeft(Map.empty[AddressId, Long]) {
        case (map, ((addressId, h), balance)) =>
          if (h <= height) map + (addressId -> balance)
          else map
      }

      byAddressId.map(kv => db.get(Keys.idToAddress(kv._1)) -> kv._2)
    }

    Either.cond(
      height > canGetAfterHeight,
      createMap(),
      GenericError(s"Cannot get waves distribution at height less than ${canGetAfterHeight + 1}")
    )
  }

  private def loadFullPortfolio(db: ReadOnlyDB, addressId: AddressId) = loadLposPortfolio(db, addressId).copy(
    assets = readFromStartForAddress(db)(Keys.AssetBalancePrefix, addressId).foldLeft(Map.empty[IssuedAsset, Long]) { (map, e) =>
      val (_, _, bs, _) = Keys.parseAddressBytesHeight(e.getKey)
      val (txH, txN)    = Keys.parseHeightNum(bs)
      val tx            = getTransactionByHN(txH, txN)
      map + (IssuedAsset(tx.id()) -> Longs.fromByteArray(e.getValue))
    }
  )

  private def loadPortfolioWithoutNFT(db: ReadOnlyDB, addressId: AddressId) = loadLposPortfolio(db, addressId).copy(
    assets = readFromStartForAddress(db)(Keys.AssetBalancePrefix, addressId).foldLeft(Map.empty[IssuedAsset, Long]) { (map, e) =>
      val (_, _, bs, _) = Keys.parseAddressBytesHeight(e.getKey)
      val (txH, txN)    = Keys.parseHeightNum(bs)
      val tx            = getTransactionByHN(txH, txN)
      val isNFT = tx match {
        case it: IssueTransaction => it.isNFT
        case _                    => false
      }
      if (isNFT) map else map + (IssuedAsset(tx.id()) -> Longs.fromByteArray(e.getValue))
    }
  )

  override protected def loadPortfolio(address: Address): Portfolio = readOnly { db =>
    val excludeNFT = this.isFeatureActivated(BlockchainFeatures.ReduceNFTFee, height)

    addressId(address).fold(Portfolio.empty) { addressId =>
      if (excludeNFT) loadPortfolioWithoutNFT(db, AddressId @@ addressId)
      else loadFullPortfolio(db, addressId)
    }
  }
}

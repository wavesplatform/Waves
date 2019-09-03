package com.wavesplatform.database.extensions.impl

import cats.effect.Resource
import com.wavesplatform.account.Address
import com.wavesplatform.database.{Keys, LevelDBWriter}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.Distributions
import com.wavesplatform.state.{AddressId, AssetDistribution, AssetDistributionPage}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.utils.Paged
import monix.eval.Task
import monix.reactive.Observable

private final class LevelDBDistributions(ldb: LevelDBWriter) extends Distributions {
  import ldb._

  def nftObservable(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] = {
    def openIterator() = readOnlyNoClose { (snapshot, db) =>
      def issueTxIterator = {
        val assetIds = db
          .get(Keys.addressId(address))
          .fold(Seq.empty[IssuedAsset]) { id =>
            val addressId = AddressId @@ id
            db.get(Keys.assetList(addressId))
          }

        assetIds.iterator
          .flatMap(ia => transactionInfo(ia.id).map(_._2))
          .collect {
            case itx: IssueTransaction if itx.isNFT => itx
          }
      }

      val result = from
        .flatMap(ia => transactionInfo(ia.id))
        .fold(issueTxIterator) {
          case (_, afterTx) =>
            issueTxIterator
              .dropWhile(_.id() != afterTx.id())
              .drop(1)
        }

      (result, snapshot)
    }

    val resource = Resource(Task {
      val (iter, snapshot) = openIterator()
      (iter, Task(snapshot.close()))
    })
    Observable.fromIterator(resource)
  }

  override def assetDistribution(asset: IssuedAsset): AssetDistribution = readOnly { db =>
    val dst = (for {
      seqNr     <- (1 to db.get(Keys.addressesForAssetSeqNr(asset))).par
      addressId <- db.get(Keys.addressesForAsset(asset, seqNr)).par
      actualHeight <- db
        .get(Keys.assetBalanceHistory(addressId, asset))
        .filterNot(_ > height)
        .headOption
      balance = db.get(Keys.assetBalance(addressId, asset)(actualHeight))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    AssetDistribution(dst)
  }

  override def assetDistributionAtHeight(
      asset: IssuedAsset,
      height: Int,
      count: Int,
      fromAddress: Option[Address]
  ): Either[ValidationError, AssetDistributionPage] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    lazy val maybeAddressId = fromAddress.flatMap(addr => db.get(Keys.addressId(addr)))

    def takeAfter(s: Seq[BigInt], a: Option[BigInt]): Seq[BigInt] = {
      a match {
        case None    => s
        case Some(v) => s.dropWhile(_ != v).drop(1)
      }
    }

    lazy val addressIds: Seq[BigInt] = {
      val all = for {
        seqNr <- 1 to db.get(Keys.addressesForAssetSeqNr(asset))
        addressId <- db
          .get(Keys.addressesForAsset(asset, seqNr))
      } yield addressId

      takeAfter(all, maybeAddressId)
    }

    lazy val distribution: Stream[(Address, Long)] =
      for {
        addressId <- addressIds.toStream
        history = db.get(Keys.assetBalanceHistory(addressId, asset))
        actualHeight <- history.filterNot(_ > height).headOption
        balance = db.get(Keys.assetBalance(addressId, asset)(actualHeight))
        if balance > 0
      } yield db.get(Keys.idToAddress(addressId)) -> balance

    lazy val page: AssetDistributionPage = {
      val dst = distribution.take(count + 1)

      val hasNext = dst.length > count
      val items   = if (hasNext) dst.init else dst
      val lastKey = items.lastOption.map(_._1)

      val result: Paged[Address, AssetDistribution] =
        Paged(hasNext, lastKey, AssetDistribution(items.toMap))

      AssetDistributionPage(result)
    }

    Either
      .cond(
        height > canGetAfterHeight,
        page,
        GenericError(s"Cannot get asset distribution at height less than ${canGetAfterHeight + 1}")
      )
  }

  override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]] = readOnly { db =>
    val canGetAfterHeight = db.get(Keys.safeRollbackHeight)

    def createMap() =
      (for {
        seqNr     <- (1 to db.get(Keys.addressesForWavesSeqNr)).par
        addressId <- db.get(Keys.addressesForWaves(seqNr)).par
        history = db.get(Keys.wavesBalanceHistory(addressId))
        actualHeight <- history.partition(_ > height)._2.headOption
        balance = db.get(Keys.wavesBalance(addressId)(actualHeight))
        if balance > 0
      } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq

    Either.cond(
      height > canGetAfterHeight,
      createMap(),
      GenericError(s"Cannot get waves distribution at height less than ${canGetAfterHeight + 1}")
    )
  }

}

object LevelDBDistributions {
  def apply(ldb: LevelDBWriter): Distributions = new LevelDBDistributions(ldb)
}

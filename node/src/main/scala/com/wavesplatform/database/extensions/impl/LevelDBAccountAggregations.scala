package com.wavesplatform.database.extensions.impl

import com.wavesplatform.account.Address
import com.wavesplatform.database.{Keys, LevelDBWriter, ReadOnlyDB}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.extensions.AccountAggregations
import com.wavesplatform.state.{AddressId, Portfolio}
import com.wavesplatform.transaction.assets.IssueTransaction

private final class LevelDBAccountAggregations(ldb: LevelDBWriter) extends AccountAggregations {
  import LevelDBWriter._
  import com.wavesplatform.features.FeatureProvider.FeatureProviderExt
  import ldb._

  def portfolio(a: Address): Portfolio =
    readOnly { db =>
      def loadFullPortfolio(db: ReadOnlyDB, addressId: BigInt) = loadLposPortfolio(db, addressId).copy(
        assets = (for {
          asset <- db.get(Keys.assetList(addressId))
        } yield asset -> db.fromHistory(Keys.assetBalanceHistory(addressId, asset), Keys.assetBalance(addressId, asset)).getOrElse(0L)).toMap
      )

      def loadPortfolioWithoutNFT(db: ReadOnlyDB, addressId: AddressId) = loadLposPortfolio(db, addressId).copy(
        assets = (for {
          issuedAsset <- db.get(Keys.assetList(addressId))
          asset <- transactionInfo(issuedAsset.id).collect {
            case (_, it: IssueTransaction) if !it.isNFT => issuedAsset
          }
        } yield asset -> db.fromHistory(Keys.assetBalanceHistory(addressId, asset), Keys.assetBalance(addressId, asset)).getOrElse(0L)).toMap
      )

      val excludeNFT = ldb.isFeatureActivated(BlockchainFeatures.ReduceNFTFee, height)

      addressId(a).fold(Portfolio.empty) { addressId =>
        if (excludeNFT) loadPortfolioWithoutNFT(db, AddressId @@ addressId)
        else loadFullPortfolio(db, addressId)
      }
    }

  override def accountDataKeys(address: Address): Set[String] = readOnly { db =>
    (for {
      addressId <- addressId(address).toVector
      keyChunkCount = db.get(Keys.dataKeyChunkCount(addressId))
      chunkNo <- Range(0, keyChunkCount)
      key     <- db.get(Keys.dataKeyChunk(addressId, chunkNo))
    } yield key).toSet
  }
}

object LevelDBAccountAggregations {
  def apply(ldb: LevelDBWriter): AccountAggregations = new LevelDBAccountAggregations(ldb)
}

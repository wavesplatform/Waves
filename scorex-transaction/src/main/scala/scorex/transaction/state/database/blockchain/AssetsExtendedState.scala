package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.transaction._
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

class AssetsExtendedState(db: MVStore) extends ScorexLogging {
  private val quantitiesTable: MVMap[Array[Byte], Long] = db.openMap(AssetsExtendedState.QuantitiesTableName,
    new LogMVMapBuilder[Array[Byte], Long])

  private val reissuableTable: MVMap[Array[Byte], Boolean] = db.openMap(AssetsExtendedState.ReissuableTableName,
    new LogMVMapBuilder[Array[Byte], Boolean])


  def updateAssetQuantity(assetId: AssetId, quantity: Long): Long = {
    val current: Long = getAssetQuantity(assetId)
    val updated: Long = Option(Math.addExact(current, quantity)).getOrElse(0L)

    Option(quantitiesTable.put(assetId, updated)).getOrElse(0L)
  }

  def setReissuable(assetId: AssetId, reissuable: Boolean): Boolean = {
    Option(reissuableTable.put(assetId, reissuable)).getOrElse(false)
  }

  def getAssetQuantity(assetId: AssetId): Long = Option(quantitiesTable.get(assetId)).getOrElse(0L)

  def isReissuable(assetId: AssetId): Boolean = Option(reissuableTable.get(assetId)).getOrElse(false)
}

object AssetsExtendedState {
  val QuantitiesTableName = "AssetsQuantities"
  val ReissuableTableName = "AssetsReissuable"
}

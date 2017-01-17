package scorex.transaction.state.database.state.storage

import org.h2.mvstore.{MVMap, MVStore}

import scala.collection.JavaConversions._
import scala.util.Try

trait MVStoreOrderMatchStorage extends OrderMatchStorageI {
  val db: MVStore

  // ============= Saved days
  val OrderMatchDays = "OrderMatchSavedDays"
  private lazy val savedDays: MVMap[Long, Boolean] = db.openMap(OrderMatchDays)

  def putSavedDays(day: Long): Unit = savedDays.put(day, true)

  def containsSavedDays(day: Long): Boolean = Option(savedDays.get(day)).isDefined

  def savedDaysKeys: List[Long] = savedDays.keyList().toList


  // ============= Order match
  val OrderMatchTx = "OrderMatchTx"

  /**
    * Returns Map Order id -> OrderMatch transactions Ids by Timestamp - starting of the day
    */
  private def orderMatchTxByDay(orderTimestamp: Long): MVMap[String, Array[String]] =
  db.openMap(OrderMatchTx + orderTimestamp)

  def getOrderMatchTxByDay(orderTimestamp: Long, key: String): Option[Array[String]] =
    Option(orderMatchTxByDay(orderTimestamp).get(key))

  def putOrderMatchTxByDay(orderTimestamp: Long, key: String, data: Array[String]): Unit =
    orderMatchTxByDay(orderTimestamp).put(key, data)

  def removeOrderMatchDays(days: List[Long]): Unit = days.foreach { d =>
    Try(db.removeMap(orderMatchTxByDay(d)))
  }
}

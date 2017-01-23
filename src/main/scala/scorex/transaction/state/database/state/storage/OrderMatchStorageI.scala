package scorex.transaction.state.database.state.storage

trait OrderMatchStorageI {
  def putSavedDays(day: Long): Unit

  def containsSavedDays(day: Long): Boolean

  def savedDaysKeys: List[Long]

  def getOrderMatchTxByDay(orderTimestamp: Long, key: String): Option[Array[String]]

  def putOrderMatchTxByDay(orderTimestamp: Long, key: String, data: Array[String]): Unit

  def removeOrderMatchDays(days: List[Long]): Unit
}

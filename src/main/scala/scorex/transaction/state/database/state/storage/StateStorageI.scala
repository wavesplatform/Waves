package scorex.transaction.state.database.state.storage

import scorex.transaction._
import scorex.transaction.state.database.state._

trait StateStorageI {

  def putTransaction(tx: Transaction, height: Int): Unit

  def removeTransaction(id: Array[Byte]): Unit

  def included(id: Array[Byte], heightOpt: Option[Int]): Option[Int]

  def getTransactionBytes(id: Array[Byte]): Option[Array[Byte]]

  def getTransaction(id: Array[Byte]): Option[Transaction] = getTransactionBytes(id)
    .flatMap(b => TransactionParser.parseBytes(b).toOption)

  def getLastStates(a: AddressString): Option[Int]

  def putLastStates(a: AddressString, s: Int): Unit

  def lastStatesKeys: List[AddressString]

  def stateHeight: Int

  def setStateHeight(height: Int): Unit

  def updateAccountAssets(address: AddressString, assetId: Option[AssetId]): Unit

  def getAccountAssets(address: AddressString): Set[String]

  def getAccountChanges(key: AddressString, height: Int): Option[Row]

  def putAccountChanges(key: AddressString, height: Int, data: Row): Unit

  def removeAccountChanges(key: AddressString, height: Int): Row

  def assetDistribution(assetId: Array[Byte]): Map[String, Long]
}

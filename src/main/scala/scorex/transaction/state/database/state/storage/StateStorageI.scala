package scorex.transaction.state.database.state.storage

import scorex.transaction._
import scorex.transaction.state.database.state._

trait StateStorageI {

  def putTransaction(tx: Transaction, height: Int): Unit

  def removeTransaction(id: Array[Byte]): Unit

  def included(id: Array[Byte]): Option[Int]

  def getTransactionBytes(id: Array[Byte]): Option[Array[Byte]]

  def getLastStates(a: Address): Option[Int]

  def putLastStates(a: Address, s: Int): Unit

  def lastStatesKeys: List[Address]

  def stateHeight: Int

  def setStateHeight(height: Int): Unit

  def updateAccountAssets(address: Address, assetId :AssetId): Unit

  def accountAssets(address: Address): Set[String]

  def getAccountChanges(key: Address, height: Int): Option[Row]

  def putAccountChanges(key: Address, height: Int, data: Row): Unit

  def removeAccountChanges(key: Address, height: Int): Row

  def accountAssets(): Map[String, Set[String]]
}

object StateStorageI {

  implicit def richStateStorageI(s: StateStorageI) = new RichStateStorageI(s)

  class RichStateStorageI(s: StateStorageI) {

    def getTransaction(id: Array[Byte]): Option[Transaction] = s.getTransactionBytes(id)
      .flatMap(b => TypedTransaction.parseBytes(b).toOption)
  }

}
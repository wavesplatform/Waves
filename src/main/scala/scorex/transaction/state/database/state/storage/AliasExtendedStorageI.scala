package scorex.transaction.state.database.state.storage

import scorex.account.Alias
import scorex.transaction.state.database.state.Address


trait AliasExtendedStorageI {
  def addressByAlias(alias: String): Option[Address]

  def persistAlias(address: Address, alias: String): Unit
  def removeAlias(alias: String): Unit
}
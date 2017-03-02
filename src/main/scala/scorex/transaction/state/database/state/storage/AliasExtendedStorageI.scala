package scorex.transaction.state.database.state.storage

import scorex.transaction.state.database.state.Address


trait AliasExtendedStorageI {
  def resolveAlias(alias: String): Option[Address]

  def persistAlias(address: Address, alias: String): Unit
}
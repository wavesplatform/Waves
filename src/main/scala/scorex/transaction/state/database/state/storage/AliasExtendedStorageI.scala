package scorex.transaction.state.database.state.storage


trait AliasExtendedStorageI {
  def addressByAlias(address: String): Option[String]
  def aliasByAddress(alias: String): Option[String]

  def persistAlias(address: String, alias: String): Unit
  def removeAlias(alias: String): Unit
}
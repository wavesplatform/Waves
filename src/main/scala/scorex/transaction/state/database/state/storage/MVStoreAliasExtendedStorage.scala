package scorex.transaction.state.database.state.storage

import org.h2.mvstore.{MVMap, MVStore}
import scorex.transaction.state.database.state.Address
import scorex.utils.LogMVMapBuilder

trait MVStoreAliasExtendedStorage extends AliasExtendedStorageI {
  val db: MVStore

  private lazy val AliasAddressTableName = "AliasAddressMap"
  private lazy val aliasAddressMap: MVMap[String, String] = db.openMap(AliasAddressTableName,
    new LogMVMapBuilder[String, String])

  def resolveAlias(alias: String): Option[Address] = Option(aliasAddressMap.get(alias))

  def persistAlias(address: Address, alias: String): Unit = aliasAddressMap.put(alias, address)

}

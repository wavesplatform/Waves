package scorex.transaction.state.database.state.storage

import org.h2.mvstore.{MVMap, MVStore}
import scorex.transaction.state.database.state.AddressString
import scorex.utils.LogMVMapBuilder

trait MVStoreAliasExtendedStorage extends AliasExtendedStorageI {
  val db: MVStore

  private lazy val AliasAddressTableName = "AliasAddressMap"
  private lazy val aliasAddressMap: MVMap[String, String] = db.openMap(AliasAddressTableName,
    new LogMVMapBuilder[String, String])


  private lazy val AddressAliasTableName = "AddressAliasMap"
  private lazy val addressAliasMap: MVMap[String, String] = db.openMap(AddressAliasTableName,
    new LogMVMapBuilder[String, String])


  def addressByAlias(alias: String): Option[AddressString] = Option(aliasAddressMap.get(alias))

  def aliasByAddress(address: String): Option[AddressString] = Option(addressAliasMap.get(address))

  def persistAlias(address: AddressString, alias: String): Unit = {
    aliasAddressMap.put(alias, address)
    addressAliasMap.put(address, alias)
  }

  def removeAlias(alias: String): Unit = {
    addressByAlias(alias) match {
      case Some(address) =>
        aliasAddressMap.remove(alias)
        addressAliasMap.remove(address)
      case None => ()
    }  }
}

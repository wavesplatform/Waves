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
  // value is aliases concatenated with `\n` separator
  private lazy val addressAliasesMap: MVMap[String, String] = db.openMap(AddressAliasTableName,
    new LogMVMapBuilder[String, String])


  def addressByAlias(alias: String): Option[AddressString] = Option(aliasAddressMap.get(alias))

  def aliasesByAddress(address: String): Seq[String] = Option(addressAliasesMap.get(address))
    .map(_.split('\n').toSeq)
    .getOrElse(Seq.empty)

  def persistAlias(address: AddressString, alias: String): Unit = {
    val allAliases = (alias +: aliasesByAddress(address)).mkString("\n")
    addressAliasesMap.put(address, allAliases)
    aliasAddressMap.put(alias, address)
  }

  def removeAlias(alias: String): Unit = {
    addressByAlias(alias) match {
      case Some(address) =>
        aliasAddressMap.remove(alias)
        val aliases = aliasesByAddress(address).filterNot(_ == alias).mkString("\n")
        addressAliasesMap.put(address, aliases)
      case None => ()
    }
  }
}

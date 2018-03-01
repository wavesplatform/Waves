package com.wavesplatform

import java.util.Properties
import javax.sql.DataSource

import com.typesafe.config.Config
import com.wavesplatform.settings.Constants
import com.wavesplatform.state2._
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import org.flywaydb.core.Flyway
import scalikejdbc.Binders
import scorex.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}

package object database {
  private def fillFromConfig(config: Config, path: String, initialProps: Properties): Properties =
    if (config.hasPath(path)) {
      config.entrySet().forEach(e => initialProps.setProperty(e.getKey, config.getString(e.getKey)))
      initialProps
    } else initialProps

  def createDataSource(config: Config): DataSource = {
//    GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(enabled = false)

    val hc = new HikariConfig()
    val flyway = new Flyway
    val props = new Properties()
    val jdbcUrl = config.getString("url")
    props.put("url", jdbcUrl)
    if (jdbcUrl.startsWith("jdbc:sqlite:")) {
      hc.setDataSourceClassName("org.sqlite.SQLiteDataSource")
      flyway.setLocations("db/migration/sqlite")
      fillFromConfig(config, "sqlite", props)
      // the following line is SUPER IMPORTANT: http://www.sqlite.org/pragma.html#pragma_foreign_keys
//      props.setProperty("enforceForeignKeys", "true")
      props.setProperty("lockingMode", "NORMAL")
      props.setProperty("journalMode", "TRUNCATE")
      props.setProperty("cacheSize", "-500000")
      props.setProperty("synchronous", "OFF")
      hc.setConnectionInitSql("pragma synchronous = off;")
    } else if (jdbcUrl.startsWith("jdbc:postgresql:")) {
      props.setProperty("ApplicationName", Constants.AgentName)
      props.setProperty("reWriteBatchedInserts", "true")
      hc.setDataSourceClassName("org.postgresql.ds.PGSimpleDataSource")
      flyway.setLocations("db/migration/postgresql")
      fillFromConfig(config, "postgresql", props)
    } else throw new Exception(s"Unsupported JDBC url: $jdbcUrl")

    hc.setDataSourceProperties(props)
    val hds = new HikariDataSource(hc)

    flyway.setDataSource(hds)
    flyway.migrate()
    hc.setAutoCommit(false)

    hds
  }

  implicit val byteStrBinders: Binders[ByteStr] = Binders.bytes.xmap(ByteStr(_), _.arr)

  implicit val addressBinders: Binders[Address] = Binders.bytes.xmap(Address.fromBytes(_).explicitGet(), _.bytes.arr)

  implicit val aliasBinders: Binders[Alias] = Binders.bytes.xmap(Alias.fromBytes(_).explicitGet(), _.bytes.arr)

  implicit val addressOrAlias: Binders[AddressOrAlias] = Binders.bytes.xmap(AddressOrAlias.fromBytes(_, 0).explicitGet()._1, _.bytes.arr)

  implicit val publicKeyBinders: Binders[PublicKeyAccount] = Binders.bytes.xmap(PublicKeyAccount(_), _.publicKey)
}

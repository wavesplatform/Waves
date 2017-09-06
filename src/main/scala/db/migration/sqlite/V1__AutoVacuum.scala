package db.migration.sqlite

import java.sql.Connection

import org.flywaydb.core.api.migration.jdbc.JdbcMigration
import scalikejdbc._

/** This is somewhat [[https://www.sqlite.org/pragma.html#pragma_auto_vacuum tricky]]:
  * <blockquote>
  *   The database connection can be changed between full and incremental autovacuum mode at any time.
  *   However, changing from "none" to "full" or "incremental" can only occur when the
  *   database is new (no tables have yet been created) or by running the VACUUM command.
  * </blockquote>
  *
  */
class V1__AutoVacuum extends JdbcMigration {
  override def migrate(connection: Connection): Unit = {
    connection.rollback()
    connection.setAutoCommit(true)

    using(connection.createStatement()) { stm =>
      stm.execute("pragma auto_vacuum=FULL")
      stm.execute("vacuum")
    }

    connection.setAutoCommit(false)
  }
}

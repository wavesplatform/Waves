package com.wavesplatform.matcher.settings

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}

case class PostgresConnection(host: String, portNumber: Int, user: String, password: String, dataSourceClassName: String) {

  def getConfig: Config = {
    ConfigFactory
      .empty()
      .withValue("dataSource.serverName", ConfigValueFactory.fromAnyRef(host))
      .withValue("dataSource.portNumber", ConfigValueFactory.fromAnyRef(portNumber))
      .withValue("dataSource.user", ConfigValueFactory.fromAnyRef(user))
      .withValue("dataSource.password", ConfigValueFactory.fromAnyRef(password))
      .withValue("dataSourceClassName", ConfigValueFactory.fromAnyRef(dataSourceClassName))
  }
}

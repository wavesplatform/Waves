package com.wavesplatform.state2

import org.h2.mvstore.{MVMap, MVStore}

abstract class VariablesStorage(protected val db: MVStore) {
  private val variables: MVMap[String, Int] = db.openMap("variables")
  protected def putInt(key: String, value: Int): Int = variables.put(key, value)
  protected def getInt(key: String): Option[Int] = Option(variables.get(key))
}

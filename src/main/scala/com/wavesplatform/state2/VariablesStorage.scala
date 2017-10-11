package com.wavesplatform.state2

import org.h2.mvstore.{MVMap, MVStore}

trait VariablesStorage {
  val db: MVStore
  private lazy val variables: MVMap[String, Int] = db.openMap("variables")
  def putInt(key: String, value: Int): Int = variables.put(key, value)
  def getInt(key: String): Option[Int] = Option(variables.get(key))
}

trait Versioned[T] {
  val codeVersion: Int
  val versionFieldKey : String
}
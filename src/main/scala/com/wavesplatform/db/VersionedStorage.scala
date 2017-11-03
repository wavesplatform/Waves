package com.wavesplatform.db

trait VersionedStorage {
  this: PropertiesStorage =>

  private val VersionProperty = "version"

  protected val Version: Int

  def isVersionValid: Boolean = getInt(VersionProperty) match {
    case None =>
      putInt(VersionProperty, Version)
      true
    case Some(v) => v == Version
  }
}

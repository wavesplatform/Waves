package com.wavesplatform.db

trait VersionedStorage {
  this: PropertiesStorage =>

  private val VersionProperty = "version"

  protected val Version: Int

  def isVersionValid: Boolean = getIntProperty(VersionProperty) match {
    case None =>
      putIntProperty(VersionProperty, Version)
      true
    case Some(v) => v == Version
  }
}

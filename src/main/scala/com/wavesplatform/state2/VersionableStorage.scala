package com.wavesplatform.state2

trait VersionableStorage {
  this: VariablesStorage =>

  protected val Version: Int
  private val stateVersion = "stateVersion"

  def isVersionValid: Boolean =
    getInt(stateVersion) match {
      case None =>
        putInt(stateVersion, Version)
        true
      case Some(v) => v == Version
    }
}

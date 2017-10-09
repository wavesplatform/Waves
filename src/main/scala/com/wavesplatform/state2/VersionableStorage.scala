package com.wavesplatform.state2

trait VersionableStorage {
  this: VariablesStorage =>

  protected val Version: Option[Int]
  private val stateVersion = "stateVersion"

  def isVersionValid: Boolean =
    Version match {
      case None => true
      case Some(version) => getInt(stateVersion) match {
        case None =>
          putInt(stateVersion, version)
          db.commit()
          true
        case Some(v) => v == version
      }
    }
}

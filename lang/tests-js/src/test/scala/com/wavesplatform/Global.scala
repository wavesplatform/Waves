package com.wavesplatform

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.ArrayBuffer

object Global {
  @JSExportTopLevel("blake2b256")
  def blake2b256(message: ArrayBuffer): ArrayBuffer = message

  @JSExportTopLevel("keccak256")
  def keccak256(message: ArrayBuffer): ArrayBuffer = message
}

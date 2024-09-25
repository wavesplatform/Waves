package com.wavesplatform.lang.v1.repl.node.http

import scala.annotation.meta.field
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("NodeConnectionSettings")
case class NodeConnectionSettings(
    @(JSExport @field) url: String,
    @(JSExport @field) chainId: Byte,
    @(JSExport @field) address: String
) {
  private val endSlash      = "(/*)$".r
  def normalizedUrl: String = endSlash.replaceAllIn(url, "")
}

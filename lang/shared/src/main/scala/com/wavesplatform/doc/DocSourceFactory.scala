package com.wavesplatform.doc

import reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object DocSourceFactory {
  private val toolbox = currentMirror.mkToolBox()
  private val path = "lang/doc-data.json"
  private val classPackage = getClass.getPackage.getName

  lazy val instance: DocSource = toolbox.eval(toolbox.parse(s"""new $classPackage.DocSource("$path")"""))
    .asInstanceOf[DocSource]
}

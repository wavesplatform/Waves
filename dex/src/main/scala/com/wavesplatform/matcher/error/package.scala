package com.wavesplatform.matcher

package object error {
  implicit class ErrorInterpolator(private val sc: StringContext) {
    def e(args: (Symbol, Any)*): MatcherErrorMessage = {
      val parts = sc.parts.init

      val (message, template, params) = parts.zipWithIndex.foldLeft(("", "", Map.empty[String, String])) {
        case ((m, t, p), (x, i)) =>
          val (argName, argValue) = args(i)
          val strValue            = Option(argValue).getOrElse("<null>").toString
          (s"$m$x$strValue", s"$t$x{{${argName.name}}}", p + (argName.name -> strValue))
      }

      MatcherErrorMessage(
        (message + sc.parts.last).trim,
        (template + sc.parts.last).trim,
        params
      )
    }
  }
}

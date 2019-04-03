package com.wavesplatform.it.sync.matcher

package object smartcontracts {
  val sc1 = s"""true"""
  val sc2 =
    s"""
       |match tx {
       | case s : SetScriptTransaction => true
       | case _ => false
       |}""".stripMargin

}

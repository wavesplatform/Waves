package com.wavesplatform.lang.contract.meta

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.Types.FINAL

import scala.collection.immutable.ListMap

trait TypesTree {
  val FieldName = "callableFuncTypes"

  def textMap(data: List[List[FINAL]], dApp: DApp): Dic = {
    val argsWithTypes = (data zip dApp.callableFuncs.map(_.u.args))
      .map { case (types, args) => args zip types }

    val funcTypesJson = argsWithTypes.map(typedArgs =>
      Dic(
        ListMap(typedArgs : _*).mapValues(t => Single(t.name))
      )
    )
    Dic(Map(FieldName -> Chain(funcTypesJson)))
  }
}

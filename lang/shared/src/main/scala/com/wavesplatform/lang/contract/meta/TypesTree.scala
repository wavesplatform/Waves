package com.wavesplatform.lang.contract.meta

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.Types.FINAL

trait TypesTree {
  val CallableFuncTypesField = "callableFuncTypes"

  def textMap(data: List[List[FINAL]], dApp: DApp): Dic = {
    val argsWithTypes = (data zip dApp.callableFuncs.map(_.u.args))
      .map { case (types, args) => args zip types }

    val funcTypesJson = argsWithTypes.map { typedArgs =>
      val functionArgs =
        typedArgs.map {
          case (arg, t) => Dic(Map("name" -> Str(arg), "type" -> Str(t.toString)))
        }
      Chain(functionArgs)
    }

    Dic(
      Map(
        CallableFuncTypesField -> Chain(funcTypesJson),
        "isArrayArguments"     -> Bool(true)
      )
    )
  }
}

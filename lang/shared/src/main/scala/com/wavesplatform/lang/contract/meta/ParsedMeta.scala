package com.wavesplatform.lang.contract.meta
import com.wavesplatform.lang.v1.compiler.Types.FINAL

case class ParsedMeta(
    version: Int,
    callableFuncTypes: Option[List[List[FINAL]]]
)

case class FunctionSignatures(
    version: Int,
    argsWithFuncName: List[(String, List[(String, FINAL)])]
)

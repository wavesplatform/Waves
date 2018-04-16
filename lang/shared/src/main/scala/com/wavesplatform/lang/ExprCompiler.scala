package com.wavesplatform.lang

import com.wavesplatform.lang.directives.Directive

trait ExprCompiler extends Versioned {
  def compile(input: String, directives: List[Directive]): Either[String, version.ExprT]
}

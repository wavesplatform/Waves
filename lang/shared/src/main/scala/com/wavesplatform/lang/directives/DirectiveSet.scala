package com.wavesplatform.lang.directives

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values._

case class DirectiveSet private (
  stdLibVersion: StdLibVersion,
  scriptType:    ScriptType,
  contentType:   ContentType,
  imports:       Imports
)

object DirectiveSet {
  val contractDirectiveSet = new DirectiveSet(V3, Account, DApp, Imports(Nil))

  def apply(
      stdLibVersion: StdLibVersion,
      scriptType:    ScriptType,
      contentType:   ContentType,
      imports:       Imports = Imports(Nil)
  ): Either[ExecutionError, DirectiveSet] =
    (stdLibVersion, scriptType, contentType, imports) match {
      case (V3, Account, DApp, _)              => Right(contractDirectiveSet)
      case (v, sType, Expression, _)           => Right(new DirectiveSet(v, sType, Expression, imports))
      case (v, sType, Library, i@Imports(Nil)) => Right(new DirectiveSet(v, sType, Library, i))
      case (_,     _, Library, _)              => Left("Libraries should not contain imports")
      case wrongSet                            => Left(errorMsg(wrongSet))
    }

  private def errorMsg(wrongSet: (StdLibVersion, ScriptType, ContentType, Imports)) = {
    val (ver, sType, cType, _) = wrongSet
    s"Inconsistent set of directives ${(ver, sType, cType)} " +
    s"could be (V3, ACCOUNT, DAPP), (<any>, <any>, LIBRARY) or (<any>, <any>, EXPRESSION)"
  }
}

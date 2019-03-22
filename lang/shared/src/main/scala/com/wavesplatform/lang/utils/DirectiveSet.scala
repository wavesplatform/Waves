package com.wavesplatform.lang.utils
import com.wavesplatform.lang.ContentType.{ContentType, DApp, Expression}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.ScriptType.{Account, ScriptType}
import com.wavesplatform.lang.StdLibVersion.{StdLibVersion, V3}

case class DirectiveSet private (
  stdLibVersion: StdLibVersion,
  scriptType:    ScriptType,
  contentType:   ContentType
)

object DirectiveSet {
  val contractDirectiveSet = new DirectiveSet(V3, Account, DApp)

  def apply(
      stdLibVersion: StdLibVersion,
      scriptType:    ScriptType,
      contentType:   ContentType
  ): Either[ExecutionError, DirectiveSet] =
    (stdLibVersion, scriptType, contentType) match {
      case (V3, Account, DApp)      => Right(contractDirectiveSet)
      case (version, sType, Expression) => Right(new DirectiveSet(version, sType, Expression))
      case wrongSet                     =>  Left(errorMsg(wrongSet))
    }

  private def errorMsg(wrongSet: (StdLibVersion, ScriptType, ContentType)) =
    s"Inconsistent set of directives $wrongSet could be (V3, ACCOUNT, DAPP) or (<any>, <any>, EXPRESSION)"
}

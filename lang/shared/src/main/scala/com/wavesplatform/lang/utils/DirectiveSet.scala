package com.wavesplatform.lang.utils
import com.wavesplatform.lang.ContentType.ContentType
import com.wavesplatform.lang.ContentType.{Contract, Expression}
import com.wavesplatform.lang.ScriptType.ScriptType
import com.wavesplatform.lang.ScriptType.Account
import com.wavesplatform.lang.{ContentType, ExecutionError, ScriptType, StdLibVersion}
import com.wavesplatform.lang.StdLibVersion.V3
import com.wavesplatform.lang.StdLibVersion.StdLibVersion

case class DirectiveSet private (
  stdLibVersion: StdLibVersion,
  scriptType:    ScriptType,
  contentType:   ContentType
)

object DirectiveSet {
  val contractDirectiveSet = new DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.Contract)

  def apply(
      stdLibVersion: StdLibVersion,
      scriptType:    ScriptType,
      contentType:   ContentType
  ): Either[ExecutionError, DirectiveSet] =
    (stdLibVersion, scriptType, contentType) match {
      case (V3, Account, Contract)      => Right(contractDirectiveSet)
      case (version, sType, Expression) => Right(new DirectiveSet(version, sType, ContentType.Expression))
      case wrongSet                     =>  Left(errorMsg(wrongSet))
    }

  private def errorMsg(wrongSet: (StdLibVersion, ScriptType, ContentType)) =
    s"Inconsistent set of directives $wrongSet could be (V3, ACCOUNT, CONTRACT) or (<any>, <any>, EXPRESSION)"
}

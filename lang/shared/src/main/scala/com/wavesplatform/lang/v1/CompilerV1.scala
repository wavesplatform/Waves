package com.wavesplatform.lang.v1

import com.wavesplatform.lang.ExprCompiler
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.Directive
import com.wavesplatform.lang.v1.TypeChecker.TypeCheckerContext
import fastparse.core.Parsed.{Failure, Success}

class CompilerV1(ctx: TypeCheckerContext) extends ExprCompiler {
  override type V = V1.type
  override val version: V = V1

  override def compile(input: String, directives: List[Directive]): Either[String, version.ExprT] = {
    Parser(input) match {
      case Success(value, _) =>
        TypeChecker(ctx, value) match {
          case Left(err)   => Left(err.toString)
          case Right(expr) => Right(expr)
        }
      case f @ Failure(_, _, _) => Left(f.toString)
    }
  }
}

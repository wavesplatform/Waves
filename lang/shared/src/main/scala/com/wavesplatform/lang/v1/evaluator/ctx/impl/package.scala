package com.wavesplatform.lang.v1.evaluator.ctx

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.either.*
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.ScriptResultSource
import com.wavesplatform.lang.v1.compiler.Terms.CaseObj
import com.wavesplatform.lang.v1.compiler.Types.UNIT
import com.wavesplatform.lang.v1.evaluator.FunctionIds.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Functions.*
import com.wavesplatform.lang.{CommonError, ExecutionError}

package object impl {
  def notImplemented[F[_]: Monad, R](funcName: String, args: List[Any]): F[Either[ExecutionError, R]] =
    (CommonError(s"Can't apply (${args.map(_.getClass.getSimpleName).mkString(", ")}) to '$funcName'"): ExecutionError)
      .asLeft[R]
      .pure[F]

  lazy val unit: CaseObj = CaseObj(UNIT, Map.empty)

  def callableResultError(expected: AnyRef, actual: AnyRef, source: ScriptResultSource): String =
    s"${source.name} needs to return $expected, but got '$actual'"

  val stateDataHeaders: Set[FunctionHeader] =
    Set(getIntegerFromStateF, getBooleanFromStateF, getBinaryFromStateF, getStringFromStateF)
      .map(_.header)

  val stateDataSelfHeaders: Set[FunctionHeader] =
    Set(getIntegerFromStateSelfF, getBooleanFromStateSelfF, getBinaryFromStateSelfF, getStringFromStateSelfF)
      .map(_.header)

  val arrayDataByKeyHeaders: Set[FunctionHeader] =
    Set(DATA_LONG_FROM_ARRAY, DATA_BOOLEAN_FROM_ARRAY, DATA_BYTES_FROM_ARRAY, DATA_STRING_FROM_ARRAY)
      .map(c => Native(c))

  val arrayDataByIndexHeaders: Set[FunctionHeader] =
    Set("getInteger", "getBoolean", "getBinary", "getString")
      .map(User(_))
}

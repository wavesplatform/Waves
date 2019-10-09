package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, CaseObj}
import com.wavesplatform.lang.v1.compiler.Types.{STRING, UNION, UNIT}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{DATA_BOOLEAN_FROM_STATE, DATA_BYTES_FROM_STATE, DATA_LONG_FROM_STATE, DATA_STRING_FROM_STATE}
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, NativeFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{notImplemented, unit}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.addressOrAliasType
import com.wavesplatform.lang.v1.traits.DataType

object ContextfulFunctions {
  def getDataFromStateF(name: String, internalName: Short, dataType: DataType): BaseFunction[F] =
    NativeFunction(
      name,
      100,
      internalName,
      UNION(dataType.innerType, UNIT),
      ("addressOrAlias", addressOrAliasType),
      ("key", STRING)
    ) {
      case (addressOrAlias: CaseObj) :: CONST_STRING(k) :: Nil =>
        environmentFunctions.getData(addressOrAlias, k, dataType).map(_.flatMap {
          case None => Right(unit)
          case Some(a) =>
            a match {
              case b: ByteStr => CONST_BYTESTR(b)
              case b: Long    => Right(CONST_LONG(b))
              case b: String  => CONST_STRING(b)
              case b: Boolean => Right(CONST_BOOLEAN(b))
            }
        })
      case xs => notImplemented[F](s"$name(s: String)", xs)
    }

  val getIntegerFromStateF: BaseFunction[F] = getDataFromStateF("getInteger", DATA_LONG_FROM_STATE, DataType.Long)
  val getBooleanFromStateF: BaseFunction[F] = getDataFromStateF("getBoolean", DATA_BOOLEAN_FROM_STATE, DataType.Boolean)
  val getBinaryFromStateF: BaseFunction[F]  = getDataFromStateF("getBinary", DATA_BYTES_FROM_STATE, DataType.ByteArray)
  val getStringFromStateF: BaseFunction[F]  = getDataFromStateF("getString", DATA_STRING_FROM_STATE, DataType.String)

}

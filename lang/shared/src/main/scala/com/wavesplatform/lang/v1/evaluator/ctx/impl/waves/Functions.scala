package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.{Id, Monad}
import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{BYTESTR, LIST, LONG, STRING, UNION, UNIT, optionLong}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.{Contextful, ContextfulNativeFunction, ContextfulUserFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext, notImplemented, unit}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.{addressOrAliasType, addressType, dataEntryType, optionAddress}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import Bindings._
import Types._
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters._
import com.wavesplatform.lang.v1.traits.domain.Recipient

object Functions {
  private def getDataFromStateF(name: String, internalName: Short, dataType: DataType): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      name,
      100,
      internalName,
      UNION(dataType.innerType, UNIT),
      ("addressOrAlias", addressOrAliasType),
      ("key", STRING)
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[Terms.EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, (addressOrAlias: CaseObj) :: CONST_STRING(key) :: Nil) =>
              val environmentFunctions = new EnvironmentFunctions[F](env)
              environmentFunctions.getData(addressOrAlias, key, dataType).map(_.flatMap {
                case None => Right(unit)
                case Some(a) =>
                  a match {
                    case b: ByteStr => CONST_BYTESTR(b)
                    case b: Long    => Right(CONST_LONG(b))
                    case b: String  => CONST_STRING(b)
                    case b: Boolean => Right(CONST_BOOLEAN(b))
                  }
              })

            case (_, xs) => notImplemented[F](s"$name(s: String)", xs)
          }
      }
    }

  val getIntegerFromStateF: BaseFunction[Environment] = getDataFromStateF("getInteger", DATA_LONG_FROM_STATE, DataType.Long)
  val getBooleanFromStateF: BaseFunction[Environment] = getDataFromStateF("getBoolean", DATA_BOOLEAN_FROM_STATE, DataType.Boolean)
  val getBinaryFromStateF: BaseFunction[Environment]  = getDataFromStateF("getBinary", DATA_BYTES_FROM_STATE, DataType.ByteArray)
  val getStringFromStateF: BaseFunction[Environment]  = getDataFromStateF("getString", DATA_STRING_FROM_STATE, DataType.String)

  private def getDataFromArrayF(name: String, internalName: Short, dataType: DataType): BaseFunction[Environment] =
    NativeFunction(
      name,
      10,
      internalName,
      UNION(dataType.innerType, UNIT),
      ("data", LIST(dataEntryType)),
      ("key", STRING)
    ) {
      case ARR(data: IndexedSeq[CaseObj] @unchecked) :: CONST_STRING(key: String) :: Nil =>
        val entryValue = data
          .find(entry => Right(entry.fields("key")) == CONST_STRING(key))
          .map(_.fields("value"))
        val result: Either[ExecutionError, EVALUATED] = entryValue match {
          case Some(n: CONST_LONG) if dataType == DataType.Long         => n.asRight[ExecutionError]
          case Some(b: CONST_BOOLEAN) if dataType == DataType.Boolean   => b.asRight[ExecutionError]
          case Some(b: CONST_BYTESTR) if dataType == DataType.ByteArray => b.asRight[ExecutionError]
          case Some(s: CONST_STRING) if dataType == DataType.String     => s.asRight[ExecutionError]
          case _                                                        => unit.asRight[ExecutionError]
        }
        result
      case xs => notImplemented[Id](s"$name(s: String)", xs)
    }

  val getIntegerFromArrayF: BaseFunction[Environment] = getDataFromArrayF("getInteger", DATA_LONG_FROM_ARRAY, DataType.Long)
  val getBooleanFromArrayF: BaseFunction[Environment] = getDataFromArrayF("getBoolean", DATA_BOOLEAN_FROM_ARRAY, DataType.Boolean)
  val getBinaryFromArrayF: BaseFunction[Environment]  = getDataFromArrayF("getBinary", DATA_BYTES_FROM_ARRAY, DataType.ByteArray)
  val getStringFromArrayF: BaseFunction[Environment]  = getDataFromArrayF("getString", DATA_STRING_FROM_ARRAY, DataType.String)

  private def getDataByIndexF(name: String, dataType: DataType): BaseFunction[Environment] =
    UserFunction(
      name,
      30,
      UNION(dataType.innerType, UNIT),
      ("@data", LIST(dataEntryType)),
      ("@index", LONG)
    ) {
      LET_BLOCK(
        LET("@val", GETTER(FUNCTION_CALL(PureContext.getElement, List(REF("@data"), REF("@index"))), "value")),
        IF(
          FUNCTION_CALL(
            PureContext._isInstanceOf,
            List(REF("@val"), CONST_STRING(dataType.innerType.name).explicitGet())
          ),
          REF("@val"),
          REF("unit")
        )
      )
    }

  val getIntegerByIndexF: BaseFunction[Environment] = getDataByIndexF("getInteger", DataType.Long)
  val getBooleanByIndexF: BaseFunction[Environment] = getDataByIndexF("getBoolean", DataType.Boolean)
  val getBinaryByIndexF: BaseFunction[Environment]  = getDataByIndexF("getBinary", DataType.ByteArray)
  val getStringByIndexF: BaseFunction[Environment]  = getDataByIndexF("getString", DataType.String)

  private def secureHashExpr(xs: EXPR): EXPR = FUNCTION_CALL(
    FunctionHeader.Native(KECCAK256),
    List(
      FUNCTION_CALL(
        FunctionHeader.Native(BLAKE256),
        List(xs)
      )
    )
  )

  val addressFromPublicKeyF: BaseFunction[Environment] =
    UserFunction.withEnvironment[Environment]("addressFromPublicKey", 82, addressType, ("@publicKey", BYTESTR))(
      new ContextfulUserFunction[Environment] {
        override def apply[F[_] : Monad](env: Environment[F]): EXPR =
          FUNCTION_CALL(
            FunctionHeader.User("Address"),
            List(
              LET_BLOCK(
                LET(
                  "@afpk_withoutChecksum",
                  FUNCTION_CALL(
                    PureContext.sumByteStr,
                    List(
                      CONST_BYTESTR(ByteStr.fromBytes(EnvironmentFunctions.AddressVersion, env.chainId)).explicitGet(),
                      // publicKeyHash
                      FUNCTION_CALL(
                        PureContext.takeBytes,
                        List(
                          secureHashExpr(REF("@publicKey")),
                          CONST_LONG(EnvironmentFunctions.HashLength)
                        )
                      )
                    )
                  )
                ),
                // bytes
                FUNCTION_CALL(
                  PureContext.sumByteStr,
                  List(
                    REF("@afpk_withoutChecksum"),
                    FUNCTION_CALL(
                      PureContext.takeBytes,
                      List(
                        secureHashExpr(REF("@afpk_withoutChecksum")),
                        CONST_LONG(EnvironmentFunctions.ChecksumLength)
                      )
                    )
                  )
                )
              )
            )
          )
      }
    )

  private def removePrefixExpr(str: EXPR, prefix: String): EXPR = IF(
    FUNCTION_CALL(
      PureContext.eq,
      List(
        FUNCTION_CALL(PureContext.takeString, List(str, CONST_LONG(prefix.length))),
        CONST_STRING(prefix).explicitGet()
      )
    ),
    FUNCTION_CALL(PureContext.dropString, List(str, CONST_LONG(prefix.length))),
    str
  )

  private def verifyAddressChecksumExpr(addressBytes: EXPR): EXPR = FUNCTION_CALL(
    PureContext.eq,
    List(
      // actual checksum
      FUNCTION_CALL(PureContext.takeRightBytes, List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength))),
      // generated checksum
      FUNCTION_CALL(
        PureContext.takeBytes,
        List(
          secureHashExpr(FUNCTION_CALL(PureContext.dropRightBytes, List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength)))),
          CONST_LONG(EnvironmentFunctions.ChecksumLength)
        )
      )
    )
  )

  val addressFromStringF: BaseFunction[Environment] =
    UserFunction.withEnvironment("addressFromString", 124, optionAddress, ("@string", STRING)) {
      new ContextfulUserFunction[Environment] {
        override def apply[F[_] : Monad](env: Environment[F]): EXPR =
          LET_BLOCK(
            LET("@afs_addrBytes",
              FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(removePrefixExpr(REF("@string"), EnvironmentFunctions.AddressPrefix)))),
            IF(
              FUNCTION_CALL(
                PureContext.eq,
                List(
                  FUNCTION_CALL(PureContext.sizeBytes, List(REF("@afs_addrBytes"))),
                  CONST_LONG(EnvironmentFunctions.AddressLength)
                )
              ),
              IF(
                // version
                FUNCTION_CALL(
                  PureContext.eq,
                  List(
                    FUNCTION_CALL(PureContext.takeBytes, List(REF("@afs_addrBytes"), CONST_LONG(1))),
                    CONST_BYTESTR(ByteStr.fromBytes(EnvironmentFunctions.AddressVersion)).explicitGet()
                  )
                ),
                IF(
                  // networkByte
                  FUNCTION_CALL(
                    PureContext.eq,
                    List(
                      FUNCTION_CALL(
                        PureContext.takeBytes,
                        List(
                          FUNCTION_CALL(PureContext.dropBytes, List(REF("@afs_addrBytes"), CONST_LONG(1))),
                          CONST_LONG(1)
                        )
                      ),
                      CONST_BYTESTR(ByteStr.fromBytes(env.chainId)).explicitGet()
                    )
                  ),
                  IF(
                    verifyAddressChecksumExpr(REF("@afs_addrBytes")),
                    FUNCTION_CALL(FunctionHeader.User("Address"), List(REF("@afs_addrBytes"))),
                    REF("unit")
                  ),
                  REF("unit")
                ),
                REF("unit")
              ),
              REF("unit")
            )
          )
      }
    }

  val addressFromRecipientF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "addressFromRecipient",
      100,
      ADDRESSFROMRECIPIENT,
      addressType,
      ("AddressOrAlias", addressOrAliasType)
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] = {
          input match {
            case (_, (c@CaseObj(`addressType`, _)) :: Nil) => (c: EVALUATED).asRight[ExecutionError].pure[F]
            case (env, CaseObj(`aliasType`, fields) :: Nil) =>
              new EnvironmentFunctions(env)
                .addressFromAlias(fields("alias").asInstanceOf[CONST_STRING].s)
                .map(_.map(resolved => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(resolved.bytes).explicitGet()))))
            case (_, xs) => notImplemented[F](s"addressFromRecipient(a: AddressOrAlias)", xs)
          }
        }
      }
    }

  val stringFromAddressF: BaseFunction[Environment] =
    NativeFunction(
      "toString",
      10,
      ADDRESSTOSTRING,
      STRING,
      ("Address", addressType)
    ) {
      case CaseObj(`addressType`, fields) :: Nil =>
        CONST_STRING(fields("bytes").asInstanceOf[CONST_BYTESTR].bs.toString)
          .asInstanceOf[Either[ExecutionError, EVALUATED]]
      case xs => notImplemented[Id](s"toString(a: Address)", xs)
    }

  private def caseObjToRecipient(c: CaseObj): Recipient = c.caseType.name match {
    case addressType.name => Recipient.Address(c.fields("bytes").asInstanceOf[CONST_BYTESTR].bs)
    case aliasType.name   => Recipient.Alias(c.fields("alias").asInstanceOf[CONST_STRING].s)
    case _                => ???
  }

  val assetBalanceF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "assetBalance",
      100,
      ACCOUNTASSETBALANCE,
      LONG,
      ("addressOrAlias", addressOrAliasType),
      ("assetId", UNION(UNIT, BYTESTR))
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, (c: CaseObj) :: u :: Nil) if u == unit =>
              env.accountBalanceOf(caseObjToRecipient(c), None).map(_.map(CONST_LONG))

            case (env, (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil) =>
              env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(_.map(CONST_LONG))

            case (_, xs) => notImplemented[F](s"assetBalance(u: ByteVector|Unit)", xs)
          }
      }
    }

  val assetInfoF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "assetInfo",
      100,
      GETASSETINFOBYID,
      optionAsset,
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env.assetInfoById(id.arr).map(_.map(buildAssetInfo) match {
                case Some(result) => result.asRight[String]
                case _ => unit.asRight[String]
              })
            case (_, xs) => notImplemented[F](s"assetInfo(u: ByteVector)", xs)
          }
      }
    }

  val wavesBalanceF: BaseFunction[Environment] =
    UserFunction("wavesBalance", 109, LONG, ("@addressOrAlias", addressOrAliasType)) {
      FUNCTION_CALL(assetBalanceF.header, List(REF("@addressOrAlias"), REF("unit")))
    }

  val txHeightByIdF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transactionHeightById",
      100,
      TRANSACTIONHEIGHTBYID,
      optionLong,
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env.transactionHeightById(id.arr)
                .map(fromOptionL)
                .map(_.asRight[String])
            case (_, xs) => notImplemented[F](s"transactionHeightById(u: ByteVector)", xs)
          }
      }
  }

  val blockInfoByHeightF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "blockInfoByHeight",
      100,
      BLOCKINFOBYHEIGHT,
      UNION(UNIT, blockInfo),
      ("height", LONG)
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_LONG(height: Long) :: Nil) =>
              env.blockInfoByHeight(height.toInt)
                .map(v => fromOptionCO(v.map(Bindings.buildLastBlockInfo)))
                .map(_.asRight[ExecutionError])
            case (_, xs) => notImplemented[F](s"blockInfoByHeight(u: Int)", xs)
          }
      }
    }

  private def withExtract[C[_[_]]](f: BaseFunction[C]): BaseFunction[C] = {
    val args = f.signature.args.zip(f.args).map {
      case ((name, ty), _) => ("@" ++ name, ty)
    }
    UserFunction(
      f.name ++ ExtractedFuncPostfix,
      ExtractedFuncPrefix ++ f.header.toString,
      f.costByLibVersion,
      f.signature.result.asInstanceOf[UNION].typeList.find(_ != UNIT).get,
      args: _*
    ) {
      FUNCTION_CALL(PureContext.extract, List(FUNCTION_CALL(f.header, args.map(a => REF(a._1)).toList)))
    }
  }

  val extractedFuncs: Array[BaseFunction[Environment]] =
    Array(
      getIntegerFromStateF,
      getBooleanFromStateF,
      getBinaryFromStateF,
      getStringFromStateF,
      getIntegerFromArrayF,
      getBooleanFromArrayF,
      getBinaryFromArrayF,
      getStringFromArrayF,
      getIntegerByIndexF,
      getBooleanByIndexF,
      getBinaryByIndexF,
      getStringByIndexF,
      addressFromStringF
    ).map(withExtract)

  def txByIdF(proofsEnabled: Boolean, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transactionById",
      100,
      GETTRANSACTIONBYID,
      txByIdReturnType(proofsEnabled, version),
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env.transactionById(id.arr)
                .map(_.map(transactionObject(_, proofsEnabled, version)))
                .map(fromOptionCO)
                .map(_.asRight[String])
            case (_, xs) => notImplemented[F](s"transactionById(u: ByteVector)", xs)
          }
      }
    }

  def transferTxByIdF(proofsEnabled: Boolean, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transferTransactionById",
      100,
      TRANSFERTRANSACTIONBYID,
      UNION(buildTransferTransactionType(proofsEnabled), UNIT),
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment] {
        override def apply[F[_] : Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env.transferTransactionById(id.arr)
                .map(_.map(transactionObject(_, proofsEnabled, version)))
                .map(fromOptionCO)
                .map(_.asRight[String])

            case (_, xs) => notImplemented[F](s"transferTransactionById(u: ByteVector)", xs)
          }
      }
    }
}

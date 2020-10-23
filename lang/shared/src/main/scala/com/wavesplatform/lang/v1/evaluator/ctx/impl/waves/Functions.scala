package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.implicits._
import cats.{Id, Monad}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{BOOLEAN, BYTESTR, LIST, LONG, STRING, UNION, UNIT, optionLong}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.{scriptTransfer => _, _}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.{addressOrAliasType, addressType, commonDataEntryType, optionAddress, _}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext, notImplemented, unit}
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.evaluator.{ContextfulNativeFunction, ContextfulUserFunction}
import com.wavesplatform.lang.v1.traits.domain.{Issue, Recipient}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}

object Functions {
  private def getDataFromStateF(name: String, internalName: Short, dataType: DataType, selfCall: Boolean): BaseFunction[Environment] = {
    val resultType = UNION(dataType.innerType, UNIT)
    val args =
      if (selfCall)
        Seq(("key", STRING))
      else
        Seq(("addressOrAlias", addressOrAliasType), ("key", STRING))

    NativeFunction.withEnvironment[Environment](
      name,
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 10L, V5 -> 10L),
      internalName,
      UNION(dataType.innerType, UNIT),
      args: _*
    ) {
      def getData[F[_]: Monad](env: Environment[F], addressOrAlias: CaseObj, key: String) = {
        val environmentFunctions = new EnvironmentFunctions[F](env)
        environmentFunctions
          .getData(addressOrAlias, key, dataType)
          .map(_.flatMap {
            case None => Right(unit)
            case Some(a) =>
              a match {
                case b: ByteStr => CONST_BYTESTR(b)
                case b: Long    => Right(CONST_LONG(b))
                case b: String  => CONST_STRING(b)
                case b: Boolean => Right(CONST_BOOLEAN(b))
              }
          })
      }

      new ContextfulNativeFunction[Environment](name, resultType, args) {
        override def ev[F[_]: Monad](input: (Environment[F], List[Terms.EVALUATED])): F[Either[ExecutionError, EVALUATED]] = {
          val (env, args) = input
          (env.tthis, args) match {
            case (address: Recipient.Address, CONST_STRING(key) :: Nil) if selfCall =>
              getData(env, Bindings.senderObject(address), key)
            case (_, (addressOrAlias: CaseObj) :: CONST_STRING(key) :: Nil) =>
              getData(env, addressOrAlias, key)
            case (_, xs) =>
              notImplemented[F, EVALUATED](s"$name(s: String)", xs)
          }
        }
      }
    }
  }

  val getIntegerFromStateF: BaseFunction[Environment] = getDataFromStateF("getInteger", DATA_LONG_FROM_STATE, DataType.Long, selfCall = false)
  val getBooleanFromStateF: BaseFunction[Environment] = getDataFromStateF("getBoolean", DATA_BOOLEAN_FROM_STATE, DataType.Boolean, selfCall = false)
  val getBinaryFromStateF: BaseFunction[Environment]  = getDataFromStateF("getBinary", DATA_BYTES_FROM_STATE, DataType.ByteArray, selfCall = false)
  val getStringFromStateF: BaseFunction[Environment]  = getDataFromStateF("getString", DATA_STRING_FROM_STATE, DataType.String, selfCall = false)

  val getIntegerFromStateSelfF: BaseFunction[Environment] = getDataFromStateF("getInteger", DATA_LONG_FROM_STATE_SELF, DataType.Long, selfCall = true)
  val getBooleanFromStateSelfF: BaseFunction[Environment] = getDataFromStateF("getBoolean", DATA_BOOLEAN_FROM_STATE_SELF, DataType.Boolean, selfCall = true)
  val getBinaryFromStateSelfF: BaseFunction[Environment]  = getDataFromStateF("getBinary", DATA_BYTES_FROM_STATE_SELF, DataType.ByteArray, selfCall = true)
  val getStringFromStateSelfF: BaseFunction[Environment]  = getDataFromStateF("getString", DATA_STRING_FROM_STATE_SELF, DataType.String, selfCall = true)

  private def getDataFromArrayF(name: String, internalName: Short, dataType: DataType, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction(
      name,
      10,
      internalName,
      UNION(dataType.innerType, UNIT),
      ("data", LIST(commonDataEntryType(version))),
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
      case xs => notImplemented[Id, EVALUATED](s"$name(s: String)", xs)
    }

  def getIntegerFromArrayF(v: StdLibVersion): BaseFunction[Environment] = getDataFromArrayF("getInteger", DATA_LONG_FROM_ARRAY, DataType.Long, v)
  def getBooleanFromArrayF(v: StdLibVersion): BaseFunction[Environment] =
    getDataFromArrayF("getBoolean", DATA_BOOLEAN_FROM_ARRAY, DataType.Boolean, v)
  def getBinaryFromArrayF(v: StdLibVersion): BaseFunction[Environment] = getDataFromArrayF("getBinary", DATA_BYTES_FROM_ARRAY, DataType.ByteArray, v)
  def getStringFromArrayF(v: StdLibVersion): BaseFunction[Environment] = getDataFromArrayF("getString", DATA_STRING_FROM_ARRAY, DataType.String, v)

  private def getDataByIndexF(name: String, dataType: DataType, version: StdLibVersion): BaseFunction[Environment] =
    UserFunction(
      name,
      Map[StdLibVersion, Long](V1 -> 30L, V2 -> 30L, V3 -> 30L, V4 -> 4L),
      UNION(dataType.innerType, UNIT),
      ("@data", LIST(commonDataEntryType(version))),
      ("@index", LONG)
    ) {
      LET_BLOCK(
        LET("@val", GETTER(FUNCTION_CALL(PureContext.getElement, List(REF("@data"), REF("@index"))), "value")),
        IF(
          FUNCTION_CALL(
            FunctionHeader.Native(ISINSTANCEOF),
            List(REF("@val"), CONST_STRING(dataType.innerType.name).explicitGet())
          ),
          REF("@val"),
          REF("unit")
        )
      )
    }

  def getIntegerByIndexF(v: StdLibVersion): BaseFunction[Environment] = getDataByIndexF("getInteger", DataType.Long, v)
  def getBooleanByIndexF(v: StdLibVersion): BaseFunction[Environment] = getDataByIndexF("getBoolean", DataType.Boolean, v)
  def getBinaryByIndexF(v: StdLibVersion): BaseFunction[Environment]  = getDataByIndexF("getBinary", DataType.ByteArray, v)
  def getStringByIndexF(v: StdLibVersion): BaseFunction[Environment]  = getDataByIndexF("getString", DataType.String, v)

  private def secureHashExpr(xs: EXPR, version: StdLibVersion): EXPR =
    FUNCTION_CALL(
      FunctionHeader.Native(if (version >= V4) KECCAK256_LIM else KECCAK256),
      List(
        FUNCTION_CALL(
          FunctionHeader.Native(if (version >= V4) BLAKE256_LIM else BLAKE256),
          List(xs)
        )
      )
    )

  def addressFromPublicKeyF(version: StdLibVersion): BaseFunction[Environment] =
    UserFunction.withEnvironment[Environment](
      name = "addressFromPublicKey",
      internalName = "addressFromPublicKey",
      Map[StdLibVersion, Long](V1 -> 82L, V2 -> 82L, V3 -> 82L, V4 -> 63L),
      addressType,
      ("@publicKey", BYTESTR)
    )(
      new ContextfulUserFunction[Environment] {
        override def apply[F[_]: Monad](env: Environment[F]): EXPR =
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
                          secureHashExpr(REF("@publicKey"), version),
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
                        secureHashExpr(REF("@afpk_withoutChecksum"), version),
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

  private def verifyAddressChecksumExpr(addressBytes: EXPR, version: StdLibVersion): EXPR = FUNCTION_CALL(
    PureContext.eq,
    List(
      // actual checksum
      FUNCTION_CALL(PureContext.takeRightBytes, List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength))),
      // generated checksum
      FUNCTION_CALL(
        PureContext.takeBytes,
        List(
          secureHashExpr(
            FUNCTION_CALL(PureContext.dropRightBytes, List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength))),
            version
          ),
          CONST_LONG(EnvironmentFunctions.ChecksumLength)
        )
      )
    )
  )

  def addressFromStringF(version: StdLibVersion): BaseFunction[Environment] =
    UserFunction.withEnvironment("addressFromString", 124, optionAddress, ("@string", STRING)) {
      new ContextfulUserFunction[Environment] {
        override def apply[F[_]: Monad](env: Environment[F]): EXPR =
          LET_BLOCK(
            LET(
              "@afs_addrBytes",
              FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(removePrefixExpr(REF("@string"), EnvironmentFunctions.AddressPrefix)))
            ),
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
                    verifyAddressChecksumExpr(REF("@afs_addrBytes"), version),
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

  val addressFromStringV4: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "addressFromString",
      1,
      ADDRESSFROMSTRING_NATIVE,
      optionAddress,
      ("@string", STRING)
    ) {
      new ContextfulNativeFunction[Environment]("addressFromString", optionAddress, Seq(("@string", STRING))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_STRING(address) :: Nil) =>
              env
                .addressFromString(address)
                .fold(
                  _ => unit,
                  address => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(address.bytes).explicitGet())): EVALUATED
                )
                .asRight[ExecutionError]
                .pure[F]
            case (_, other) =>
              notImplemented[F, EVALUATED](s"addressFromString(a: String)", other)
          }
      }
    }

  val addressFromRecipientF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "addressFromRecipient",
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 5L),
      ADDRESSFROMRECIPIENT,
      addressType,
      ("AddressOrAlias", addressOrAliasType)
    ) {
      new ContextfulNativeFunction[Environment]("addressFromRecipient", addressType, Seq(("AddressOrAlias", addressOrAliasType))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] = {
          input match {
            case (_, (c @ CaseObj(`addressType`, _)) :: Nil) => (c: EVALUATED).asRight[ExecutionError].pure[F]
            case (env, CaseObj(`aliasType`, fields) :: Nil) =>
              new EnvironmentFunctions(env)
                .addressFromAlias(fields("alias").asInstanceOf[CONST_STRING].s)
                .map(_.map(resolved => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(resolved.bytes).explicitGet()))))
            case (_, xs) => notImplemented[F, EVALUATED](s"addressFromRecipient(a: AddressOrAlias)", xs)
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
      case xs => notImplemented[Id, EVALUATED](s"toString(a: Address)", xs)
    }

  private def caseObjToRecipient(c: CaseObj): Recipient = c.caseType.name match {
    case addressType.name => Recipient.Address(c.fields("bytes").asInstanceOf[CONST_BYTESTR].bs)
    case aliasType.name   => Recipient.Alias(c.fields("alias").asInstanceOf[CONST_STRING].s)
    case _                => ???
  }

  val assetBalanceF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "assetBalance",
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 10L),
      ACCOUNTASSETBALANCE,
      LONG,
      ("addressOrAlias", addressOrAliasType),
      ("assetId", UNION(UNIT, BYTESTR))
    ) {
      new ContextfulNativeFunction[Environment]("assetBalance", LONG, Seq(("addressOrAlias", addressOrAliasType), ("assetId", UNION(UNIT, BYTESTR)))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, (c: CaseObj) :: u :: Nil) if u == unit =>
              env.accountBalanceOf(caseObjToRecipient(c), None).map(_.map(CONST_LONG))

            case (env, (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil) =>
              env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(_.map(CONST_LONG))

            case (_, xs) => notImplemented[F, EVALUATED](s"assetBalance(a: Address|Alias, u: ByteVector|Unit)", xs)
          }
      }
    }

  val assetBalanceV4F: BaseFunction[Environment] = {
    val args = Seq(("addressOrAlias", addressOrAliasType), ("assetId", BYTESTR))
    NativeFunction.withEnvironment[Environment](
      "assetBalance",
      10,
      ACCOUNTASSETONLYBALANCE,
      LONG,
      args: _*
    ) {
      new ContextfulNativeFunction[Environment]("assetBalance", LONG, args) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] = {
          val (env, args) = input
          (env.tthis, args) match {
            case (_, (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil) =>
              env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(_.map(CONST_LONG))
            case (_, xs) =>
              notImplemented[F, EVALUATED](s"assetBalance(a: Address|Alias, u: ByteVector)", xs)
          }
        }
      }
    }
  }

  val wavesBalanceV4F: BaseFunction[Environment] = {
    val args = Seq(("addressOrAlias", addressOrAliasType))
    NativeFunction.withEnvironment[Environment](
      "wavesBalance",
      10,
      ACCOUNTWAVESBALANCE,
      balanceDetailsType,
      args: _*
    ) {
      def wavesBalance[F[_]: Monad](env: Environment[F], recipient: Recipient): F[Either[String, EVALUATED]] = {
        env
          .accountWavesBalanceOf(recipient)
          .map(
            _.map(
              b =>
                CaseObj(
                  balanceDetailsType,
                  Map(
                    "available"  -> CONST_LONG(b.available),
                    "regular"    -> CONST_LONG(b.regular),
                    "generating" -> CONST_LONG(b.generating),
                    "effective"  -> CONST_LONG(b.effective)
                  )
                )
            )
          )
      }

      new ContextfulNativeFunction[Environment]("wavesBalance", LONG, args) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] = {
          val (env, args) = input
          (env.tthis, args) match {
            case (_, (c: CaseObj) :: Nil) =>
              wavesBalance(env, caseObjToRecipient(c))
            case (_, xs) =>
              notImplemented[F, EVALUATED](s"wavesBalance(a: Address|Alias)", xs)
          }
        }
      }
    }
  }

  def assetInfoF(version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "assetInfo",
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 15L),
      GETASSETINFOBYID,
      optionAsset(version),
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment]("assetInfo", optionAsset(version), Seq(("id", BYTESTR))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env
                .assetInfoById(id.arr)
                .map(_.map(buildAssetInfo(_, version)) match {
                  case Some(result) => result.asRight[String]
                  case _            => unit.asRight[String]
                })
            case (_, xs) => notImplemented[F, EVALUATED](s"assetInfo(u: ByteVector)", xs)
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
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 20L),
      TRANSACTIONHEIGHTBYID,
      optionLong,
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment]("transactionHeightById", optionLong, Seq(("id", BYTESTR))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env
                .transactionHeightById(id.arr)
                .map(fromOptionL)
                .map(_.asRight[String])
            case (_, xs) => notImplemented[F, EVALUATED](s"transactionHeightById(u: ByteVector)", xs)
          }
      }
    }

  def blockInfoByHeightF(version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "blockInfoByHeight",
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 5L),
      BLOCKINFOBYHEIGHT,
      UNION(UNIT, blockInfo(version)),
      ("height", LONG)
    ) {
      new ContextfulNativeFunction[Environment]("blockInfoByHeight", UNION(UNIT, blockInfo(version)), Seq(("height", LONG))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_LONG(height: Long) :: Nil) =>
              env
                .blockInfoByHeight(height.toInt)
                .map(v => fromOptionCO(v.map(bi => Bindings.buildBlockInfo(bi, version))))
                .map(_.asRight[ExecutionError])
            case (_, xs) => notImplemented[F, EVALUATED](s"blockInfoByHeight(u: Int)", xs)
          }
      }
    }

  private def withExtract[C[_[_]]](f: BaseFunction[C], version: StdLibVersion): BaseFunction[C] = {
    val args = f.signature.args.zip(f.args).map {
      case ((name, ty), _) => ("@" ++ name, ty)
    }
    UserFunction(
      f.name ++ ExtractedFuncPostfix,
      ExtractedFuncPrefix ++ f.header.toString,
      f.costByLibVersionMap,
      f.signature.result.asInstanceOf[UNION].typeList.find(_ != UNIT).get,
      args: _*
    ) {
      val extractF = if (version >= V4) PureContext.value else PureContext.extract
      FUNCTION_CALL(extractF, List(FUNCTION_CALL(f.header, args.map(a => REF(a._1)).toList)))
    }
  }

  def extractedFuncs(v: StdLibVersion): Array[BaseFunction[Environment]] =
    Array(
      getIntegerFromStateF,
      getBooleanFromStateF,
      getBinaryFromStateF,
      getStringFromStateF,
      getIntegerFromArrayF(v),
      getBooleanFromArrayF(v),
      getBinaryFromArrayF(v),
      getStringFromArrayF(v),
      getIntegerByIndexF(v),
      getBooleanByIndexF(v),
      getBinaryByIndexF(v),
      getStringByIndexF(v),
      if (v >= V4) addressFromStringV4 else addressFromStringF(v)
    ).map(withExtract(_, v))

  def extractedStateSelfFuncs(v: StdLibVersion): Array[BaseFunction[Environment]] =
    Array(
      getIntegerFromStateSelfF,
      getBooleanFromStateSelfF,
      getBinaryFromStateSelfF,
      getStringFromStateSelfF
    ).map(withExtract(_, v))

  def txByIdF(proofsEnabled: Boolean, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transactionById",
      100,
      GETTRANSACTIONBYID,
      txByIdReturnType(proofsEnabled, version),
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment]("transactionById", txByIdReturnType(proofsEnabled, version), Seq(("id", BYTESTR))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env
                .transactionById(id.arr)
                .map(_.map(transactionObject(_, proofsEnabled, version)))
                .map(fromOptionCO)
                .map(_.asRight[String])
            case (_, xs) => notImplemented[F, EVALUATED](s"transactionById(u: ByteVector)", xs)
          }
      }
    }

  def transferTxByIdF(proofsEnabled: Boolean, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transferTransactionById",
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 60L),
      TRANSFERTRANSACTIONBYID,
      UNION(buildTransferTransactionType(proofsEnabled, version), UNIT),
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment](
        "transferTransactionById",
        UNION(buildTransferTransactionType(proofsEnabled, version), UNIT),
        Seq(("id", BYTESTR))
      ) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CONST_BYTESTR(id: ByteStr) :: Nil) =>
              env
                .transferTransactionById(id.arr)
                .map(_.map(transactionObject(_, proofsEnabled, version)))
                .map(fromOptionCO)
                .map(_.asRight[String])

            case (_, xs) => notImplemented[F, EVALUATED](s"transferTransactionById(u: ByteVector)", xs)
          }
      }
    }

  val calculateAssetIdF: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "calculateAssetId",
      10,
      CALCULATE_ASSET_ID,
      BYTESTR,
      ("issue", issueActionType)
    ) {
      new ContextfulNativeFunction[Environment]("calculateAssetId", BYTESTR, Seq(("issue", issueActionType))) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, CaseObj(`issueActionType`, fields) :: Nil) =>
              val MaxAssetNameLength        = 16
              val MaxAssetDescriptionLength = 1000
              // This constants are dublicate of IssueTransaction.* but need to limit of hash calculating complexity and dom't relate with consensus.
              // Consensus check it in InvokeScriptTransactionDiff.

              val name        = fields(FieldNames.IssueName).asInstanceOf[CONST_STRING].s
              val description = fields(FieldNames.IssueDescription).asInstanceOf[CONST_STRING].s

              (if (description.getBytes("UTF-8").length > MaxAssetDescriptionLength)
                 Left(s"Description length should not exceed $MaxAssetDescriptionLength")
               else if (name.getBytes("UTF-8").length > MaxAssetNameLength)
                 Left(s"Name length should not exceed $MaxAssetNameLength")
               else
                 CONST_BYTESTR(
                   Issue.calculateId(
                     decimals = fields(FieldNames.IssueDecimals).asInstanceOf[CONST_LONG].t.toInt,
                     description = description,
                     isReissuable = fields(FieldNames.IssueIsReissuable).asInstanceOf[CONST_BOOLEAN].b,
                     name = name,
                     quantity = fields(FieldNames.IssueQuantity).asInstanceOf[CONST_LONG].t,
                     nonce = fields(FieldNames.IssueNonce).asInstanceOf[CONST_LONG].t,
                     parent = env.txId
                   )
                 ): Either[String, EVALUATED]).pure[F]

            case (env, xs) => notImplemented[F, EVALUATED](s"calculateAssetId(i: Issue)", xs)
          }
      }
    }

  def transactionFromProtoBytesF(proofsEnabled: Boolean, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transferTransactionFromProto",
      5,
      TRANSFER_TRANSACTION_FROM_PROTO,
      UNION(buildTransferTransactionType(proofsEnabled, version), UNIT),
      ("bytes", BYTESTR)
    ) {
      new ContextfulNativeFunction[Environment](
        "transferTransactionFromProto",
        UNION(buildTransferTransactionType(proofsEnabled, version), UNIT),
        Seq(("bytes", BYTESTR))
      ) {
        override def ev[F[_]: Monad](input: (Environment[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
          input match {
            case (env, List(CONST_BYTESTR(bytes))) =>
              env
                .transferTransactionFromProto(bytes.arr)
                .map(
                  tx =>
                    (tx.map(transactionObject(_, proofsEnabled, version)): EVALUATED)
                      .asRight[ExecutionError]
                )

            case (_, xs) => notImplemented[F, EVALUATED](s"transferTransactionFromProto(bytes: ByteVector)", xs)
          }
      }
    }

  val simplifiedIssueActionConstructor: BaseFunction[Environment] =
    NativeFunction(
      "Issue",
      1,
      SIMPLIFIED_ISSUE_ACTION_CONSTRUCTOR,
      issueActionType,
      FieldNames.IssueName         -> STRING,
      FieldNames.IssueDescription  -> STRING,
      FieldNames.IssueQuantity     -> LONG,
      FieldNames.IssueDecimals     -> LONG,
      FieldNames.IssueIsReissuable -> BOOLEAN
    ) { args =>
      val typedArgs = (issueActionType.fields.map(_._1) zip (args ::: List(unit, CONST_LONG(0)))).toMap
      Right(CaseObj(issueActionType, typedArgs))
    }

  val detailedIssueActionConstructor: BaseFunction[Environment] =
    NativeFunction(
      "Issue",
      1,
      DETAILED_ISSUE_ACTION_CONSTRUCTOR,
      issueActionType,
      FieldNames.IssueName         -> STRING,
      FieldNames.IssueDescription  -> STRING,
      FieldNames.IssueQuantity     -> LONG,
      FieldNames.IssueDecimals     -> LONG,
      FieldNames.IssueIsReissuable -> BOOLEAN,
      FieldNames.IssueScriptField  -> UNION(issueScriptType, UNIT),
      FieldNames.IssueNonce        -> LONG
    ) { args =>
      val typedArgs = (issueActionType.fields.map(_._1) zip args).toMap
      Right(CaseObj(issueActionType, typedArgs))
    }
}

package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.{Id, Monad}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.FunctionIds.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.{scriptTransfer as _, *}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext, notImplemented, unit}
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.evaluator.{ContextfulNativeFunction, ContextfulUserFunction, FunctionIds}
import com.wavesplatform.lang.v1.traits.domain.{Issue, Lease, Recipient}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.{BaseGlobal, FunctionHeader}
import com.wavesplatform.lang.{FailOrRejectError, CoevalF, CommonError, ExecutionError}
import monix.eval.Coeval
import shapeless.Coproduct.unsafeGet

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
      Map[StdLibVersion, Long](V1 -> 100L, V2 -> 100L, V3 -> 100L, V4 -> 10L),
      internalName,
      UNION(dataType.innerType, UNIT),
      args*
    ) {
      def getData[F[_]: Monad](env: Environment[F], addressOrAlias: CaseObj, key: String) = {
        val environmentFunctions = new EnvironmentFunctions[F](env)
        environmentFunctions
          .getData(addressOrAlias, key, dataType)
          .map(_.flatMap {
            case None => Right(unit)
            case Some(a) =>
              (a: @unchecked) match {
                case b: ByteStr => CONST_BYTESTR(b)
                case b: Long    => Right(CONST_LONG(b))
                case b: String  => CONST_STRING(b)
                case b: Boolean => Right(CONST_BOOLEAN(b))
              }
          })
      }

      new ContextfulNativeFunction.Simple[Environment](name, resultType, args) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] = {
          (unsafeGet(env.tthis), args) match {
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
  val getBooleanFromStateSelfF: BaseFunction[Environment] =
    getDataFromStateF("getBoolean", DATA_BOOLEAN_FROM_STATE_SELF, DataType.Boolean, selfCall = true)
  val getBinaryFromStateSelfF: BaseFunction[Environment] =
    getDataFromStateF("getBinary", DATA_BYTES_FROM_STATE_SELF, DataType.ByteArray, selfCall = true)
  val getStringFromStateSelfF: BaseFunction[Environment] =
    getDataFromStateF("getString", DATA_STRING_FROM_STATE_SELF, DataType.String, selfCall = true)

  val isDataStorageUntouchedF: BaseFunction[Environment] = {
    val name       = "isDataStorageUntouched"
    val resultType = BOOLEAN
    val arg        = ("addressOrAlias", addressOrAliasType)
    NativeFunction.withEnvironment[Environment](
      name,
      Map[StdLibVersion, Long](V5 -> 10L),
      IS_UNTOUCHED,
      resultType,
      arg
    ) {
      new ContextfulNativeFunction.Simple[Environment](name, resultType, List(arg)) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case (addressOrAlias: CaseObj) :: Nil =>
              val environmentFunctions = new EnvironmentFunctions[F](env)
              environmentFunctions
                .hasData(addressOrAlias)
                .map(_.map(v => CONST_BOOLEAN(!v)))

            case xs => notImplemented[F, EVALUATED](s"$name(s: AddressOrAlias)", xs)
          }
      }
    }
  }

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
        override def apply[F[_]: Monad](env: Environment[F], startArgs: List[EXPR]): EXPR =
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
                        Native(FunctionIds.TAKE_BYTES),
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
                      Native(FunctionIds.TAKE_BYTES),
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

  val addressFromPublicKeyNative: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "addressFromPublicKey",
      Map[StdLibVersion, Long](V6 -> 1L),
      ADDRESSFROMPUBLICKEY_NATIVE,
      addressType,
      ("publicKey", BYTESTR)
    ) {
      new ContextfulNativeFunction.Simple[Environment]("addressFromPublicKey", addressType, Seq(("AddressOrAlias", addressOrAliasType))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] = {
          (env, args) match {
            case (env, CONST_BYTESTR(publicKey) :: Nil) =>
              env
                .addressFromPublicKey(publicKey)
                .bimap(
                  CommonError(_): ExecutionError,
                  address => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(address.bytes).explicitGet())): EVALUATED
                )
                .pure[F]
            case (_, xs) => notImplemented[F, EVALUATED](s"addressFromPublicKey(publicKey: ByteVector)", xs)
          }
        }
      }
    }

  private def removePrefixExpr(str: EXPR, prefix: String): EXPR = IF(
    FUNCTION_CALL(
      PureContext.eq,
      List(
        FUNCTION_CALL(Native(FunctionIds.TAKE_STRING), List(str, CONST_LONG(prefix.length))),
        CONST_STRING(prefix).explicitGet()
      )
    ),
    FUNCTION_CALL(Native(FunctionIds.DROP_STRING), List(str, CONST_LONG(prefix.length))),
    str
  )

  private def verifyAddressChecksumExpr(addressBytes: EXPR, version: StdLibVersion): EXPR = FUNCTION_CALL(
    PureContext.eq,
    List(
      // actual checksum
      FUNCTION_CALL(
        if (version >= V6) Native(FunctionIds.TAKE_RIGHT_BYTES) else User("takeRightBytes"),
        List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength))
      ),
      // generated checksum
      FUNCTION_CALL(
        Native(FunctionIds.TAKE_BYTES),
        List(
          secureHashExpr(
            FUNCTION_CALL(
              if (version >= V6) Native(FunctionIds.DROP_RIGHT_BYTES) else User("dropRightBytes"),
              List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength))
            ),
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
        override def apply[F[_]: Monad](env: Environment[F], startArgs: List[EXPR]): EXPR =
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
                    FUNCTION_CALL(Native(FunctionIds.TAKE_BYTES), List(REF("@afs_addrBytes"), CONST_LONG(1))),
                    CONST_BYTESTR(ByteStr.fromBytes(EnvironmentFunctions.AddressVersion)).explicitGet()
                  )
                ),
                IF(
                  // networkByte
                  FUNCTION_CALL(
                    PureContext.eq,
                    List(
                      FUNCTION_CALL(
                        Native(FunctionIds.TAKE_BYTES),
                        List(
                          FUNCTION_CALL(Native(FunctionIds.DROP_BYTES), List(REF("@afs_addrBytes"), CONST_LONG(1))),
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
      new ContextfulNativeFunction.Simple[Environment]("addressFromString", optionAddress, Seq(("@string", STRING))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CONST_STRING(address) :: Nil =>
              env
                .addressFromString(address)
                .fold(
                  _ => unit,
                  address => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(address.bytes).explicitGet())): EVALUATED
                )
                .asRight[ExecutionError]
                .pure[F]
            case other =>
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
      new ContextfulNativeFunction.Simple[Environment]("addressFromRecipient", addressType, Seq(("AddressOrAlias", addressOrAliasType))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] = {
          args match {
            case (c @ CaseObj(`addressType`, _)) :: Nil =>
              (c: EVALUATED).asRight[ExecutionError].pure[F]
            case CaseObj(`aliasType`, fields) :: Nil =>
              new EnvironmentFunctions(env)
                .addressFromAlias(fields("alias").asInstanceOf[CONST_STRING].s)
                .map(_.map(resolved => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(resolved.bytes).explicitGet()))))
            case xs =>
              notImplemented[F, EVALUATED](s"addressFromRecipient(a: AddressOrAlias)", xs)
          }
        }
      }
    }

  val stringFromAddressF: BaseFunction[Environment] =
    NativeFunction(
      "toString",
      Map(V3 -> 10L, V4 -> 10L, V5 -> 10L, V6 -> 1L),
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
    case t                => throw new IllegalArgumentException(s"Unexpected recipient type $t")
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
      new ContextfulNativeFunction.Simple[Environment](
        "assetBalance",
        LONG,
        Seq(("addressOrAlias", addressOrAliasType), ("assetId", UNION(UNIT, BYTESTR)))
      ) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case (c: CaseObj) :: u :: Nil if u == unit =>
              env.accountBalanceOf(caseObjToRecipient(c), None).map(_.map(CONST_LONG).leftMap(CommonError))
            case (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil =>
              env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(_.map(CONST_LONG).leftMap(CommonError))
            case xs =>
              notImplemented[F, EVALUATED](s"assetBalance(a: Address|Alias, u: ByteVector|Unit)", xs)
          }
      }
    }

  val assetBalanceV4F: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "assetBalance",
      10,
      ACCOUNTASSETONLYBALANCE,
      LONG,
      ("addressOrAlias", addressOrAliasType),
      ("assetId", BYTESTR)
    ) {
      new ContextfulNativeFunction.Simple[Environment]("assetBalance", LONG, Seq(("addressOrAlias", addressOrAliasType), ("assetId", BYTESTR))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil =>
              env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(_.map(CONST_LONG).leftMap(CommonError))
            case xs =>
              notImplemented[F, EVALUATED](s"assetBalance(a: Address|Alias, u: ByteVector)", xs)
          }
      }
    }

  val wavesBalanceV4F: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "wavesBalance",
      10,
      ACCOUNTWAVESBALANCE,
      balanceDetailsType,
      ("addressOrAlias", addressOrAliasType)
    ) {
      new ContextfulNativeFunction.Simple[Environment]("wavesBalance", LONG, Seq(("addressOrAlias", addressOrAliasType))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case (c: CaseObj) :: Nil =>
              env
                .accountWavesBalanceOf(caseObjToRecipient(c))
                .map(
                  _.map(b =>
                    CaseObj(
                      balanceDetailsType,
                      Map(
                        "available"  -> CONST_LONG(b.available),
                        "regular"    -> CONST_LONG(b.regular),
                        "generating" -> CONST_LONG(b.generating),
                        "effective"  -> CONST_LONG(b.effective)
                      )
                    )
                  ).leftMap(CommonError)
                )

            case xs => notImplemented[F, EVALUATED](s"wavesBalance(a: Address|Alias)", xs)
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
      new ContextfulNativeFunction.Simple[Environment]("assetInfo", optionAsset(version), Seq(("id", BYTESTR))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CONST_BYTESTR(id: ByteStr) :: Nil =>
              env
                .assetInfoById(id.arr)
                .map(_.map(buildAssetInfo(_, version)) match {
                  case Some(result) => result.asRight[ExecutionError]
                  case _            => unit.asRight[ExecutionError]
                })
            case xs =>
              notImplemented[F, EVALUATED](s"assetInfo(u: ByteVector)", xs)
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
      new ContextfulNativeFunction.Simple[Environment]("transactionHeightById", optionLong, Seq(("id", BYTESTR))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CONST_BYTESTR(id: ByteStr) :: Nil =>
              env
                .transactionHeightById(id.arr)
                .map(fromOptionL)
                .map(_.asRight[ExecutionError])
            case xs =>
              notImplemented[F, EVALUATED](s"transactionHeightById(u: ByteVector)", xs)
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
      new ContextfulNativeFunction.Simple[Environment]("blockInfoByHeight", UNION(UNIT, blockInfo(version)), Seq(("height", LONG))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CONST_LONG(height: Long) :: Nil =>
              env
                .blockInfoByHeight(height.toInt)
                .map(v => fromOptionCO(v.map(bi => Bindings.buildBlockInfo(bi, version))))
                .map(_.asRight[ExecutionError])
            case xs =>
              notImplemented[F, EVALUATED](s"blockInfoByHeight(u: Int)", xs)
          }
      }
    }

  def callDAppF(reentrant: Boolean): BaseFunction[Environment] = {
    val (id, name) = if (reentrant) (CALLDAPPREENTRANT, "reentrantInvoke") else (CALLDAPP, "invoke")
    NativeFunction.withEnvironment[Environment](
      name,
      Map[StdLibVersion, Long](V5 -> 75L),
      id,
      ANY,
      ("dapp", addressOrAliasType),
      ("name", optionString),
      ("args", LIST(ANY)),
      ("payments", listPayment)
    ) {
      new ContextfulNativeFunction.Extended[Environment](
        name,
        ANY,
        Seq(("dapp", BYTESTR), ("name", STRING), ("args", LIST(ANY)), ("payments", listPayment))
      ) {
        override def evaluate[F[_]: Monad](
            env: Environment[F],
            args: List[EVALUATED],
            availableComplexity: Int
        )(implicit m: Monad[CoevalF[F, *]]): Coeval[F[(Either[ExecutionError, EVALUATED], Int)]] = {
          val dAppBytes = args match {
            case (dApp: CaseObj) :: _ if dApp.caseType == addressType =>
              dApp.fields("bytes") match {
                case CONST_BYTESTR(d) => d.pure[F]
                case a                => throw new IllegalArgumentException(s"Unexpected address bytes $a")
              }
            case (dApp: CaseObj) :: _ if dApp.caseType == aliasType =>
              (dApp.fields("alias"): @unchecked) match {
                case CONST_STRING(a) => env.resolveAlias(a).map(_.explicitGet().bytes)
              }
            case args => throw new IllegalArgumentException(s"Unexpected recipient args $args")
          }
          val name = args match {
            case _ :: CONST_STRING(name) :: _ => name
            case _ :: CaseObj(UNIT, _) :: _   => "default"
            case args                         => throw new IllegalArgumentException(s"Unexpected input args $args")
          }
          args match {
            case _ :: _ :: ARR(args) :: ARR(payments) :: Nil =>
              env
                .callScript(
                  Recipient.Address(dAppBytes.asInstanceOf[ByteStr]),
                  name,
                  args.toList,
                  payments.map {
                    case p: CaseObj if p.caseType == paymentType =>
                      (List("assetId", "amount").map(p.fields): @unchecked) match {
                        case CONST_BYTESTR(a) :: CONST_LONG(v) :: Nil => (Some(a.arr), v)
                        case CaseObj(UNIT, _) :: CONST_LONG(v) :: Nil => (None, v)
                      }
                    case arg => throw new IllegalArgumentException(s"Unexpected payment arg $arg")
                  },
                  availableComplexity,
                  reentrant
                )
                .map(_.map { case (result, spentComplexity) =>
                  val mappedError = result.leftMap {
                    case reject: FailOrRejectError => reject
                    case other                     => CommonError(other.toString)
                  }
                  (mappedError, availableComplexity - spentComplexity)
                })
            case xs =>
              val err = notImplemented[F, EVALUATED](s"invoke(dApp: Address, function: String, args: List[Any], payments: List[Payment])", xs)
              Coeval.now(err.map((_, 0)))
          }
        }
      }
    }
  }

  private def withExtract[C[_[_]]](f: BaseFunction[C], version: StdLibVersion): BaseFunction[C] = {
    val args = f.signature.args.zip(f.args).map { case ((name, ty), _) =>
      ("@" ++ name, ty)
    }
    UserFunction(
      f.name ++ ExtractedFuncPostfix,
      ExtractedFuncPrefix ++ f.header.toString,
      f.costByLibVersionMap,
      f.signature.result.asInstanceOf[UNION].typeList.find(_ != UNIT).get,
      args*
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
      new ContextfulNativeFunction.Simple[Environment]("transactionById", txByIdReturnType(proofsEnabled, version), Seq(("id", BYTESTR))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CONST_BYTESTR(id: ByteStr) :: Nil =>
              env
                .transactionById(id.arr)
                .map(_.map(transactionObject(_, proofsEnabled, version)))
                .map(fromOptionCO)
                .map(_.asRight[ExecutionError])
            case xs =>
              notImplemented[F, EVALUATED](s"transactionById(u: ByteVector)", xs)
          }
      }
    }

  def transferTxByIdF(proofsEnabled: Boolean, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transferTransactionById",
      Map[StdLibVersion, Long](V3 -> 100L, V4 -> 60L),
      TRANSFERTRANSACTIONBYID,
      UNION(buildTransferTransactionType(proofsEnabled), UNIT),
      ("id", BYTESTR)
    ) {
      new ContextfulNativeFunction.Simple[Environment](
        "transferTransactionById",
        UNION(buildTransferTransactionType(proofsEnabled), UNIT),
        Seq(("id", BYTESTR))
      ) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CONST_BYTESTR(id: ByteStr) :: Nil =>
              env
                .transferTransactionById(id.arr)
                .map(_.filter(version >= V6 || _.p.h.version > 0).map(transactionObject(_, proofsEnabled, version)))
                .map(fromOptionCO)
                .map(_.asRight[ExecutionError])
            case xs =>
              notImplemented[F, EVALUATED](s"transferTransactionById(u: ByteVector)", xs)
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
      new ContextfulNativeFunction.Simple[Environment]("calculateAssetId", BYTESTR, Seq(("issue", issueActionType))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CaseObj(`issueActionType`, fields) :: Nil =>
              val MaxAssetNameLength        = 16
              val MaxAssetDescriptionLength = 1000
              // This constants are dublicate of IssueTransaction.* but need to limit of hash calculating complexity and dom't relate with consensus.
              // Consensus check it in InvokeScriptTransactionDiff.

              val name        = fields(FieldNames.IssueName).asInstanceOf[CONST_STRING].s
              val description = fields(FieldNames.IssueDescription).asInstanceOf[CONST_STRING].s

              (if (description.getBytes("UTF-8").length > MaxAssetDescriptionLength)
                 Left(CommonError(s"Description length should not exceed $MaxAssetDescriptionLength"))
               else if (name.getBytes("UTF-8").length > MaxAssetNameLength)
                 Left(CommonError(s"Name length should not exceed $MaxAssetNameLength"))
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
                 ): Either[ExecutionError, EVALUATED]).pure[F]

            case xs => notImplemented[F, EVALUATED](s"calculateAssetId(i: Issue)", xs)
          }
      }
    }

  def transactionFromProtoBytesF(proofsEnabled: Boolean, version: StdLibVersion): BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "transferTransactionFromProto",
      5,
      TRANSFER_TRANSACTION_FROM_PROTO,
      UNION(buildTransferTransactionType(proofsEnabled), UNIT),
      ("bytes", BYTESTR)
    ) {
      new ContextfulNativeFunction.Simple[Environment](
        "transferTransactionFromProto",
        UNION(buildTransferTransactionType(proofsEnabled), UNIT),
        Seq(("bytes", BYTESTR))
      ) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case List(CONST_BYTESTR(bytes)) =>
              env
                .transferTransactionFromProto(bytes.arr)
                .map(tx =>
                  (tx.map(transactionObject(_, proofsEnabled, version)): EVALUATED)
                    .asRight[ExecutionError]
                )

            case xs => notImplemented[F, EVALUATED](s"transferTransactionFromProto(bytes: ByteVector)", xs)
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

  val simplifiedLeaseActionConstructor: BaseFunction[Environment] =
    NativeFunction(
      "Lease",
      1,
      SIMPLIFIED_LEASE_ACTION_CONSTRUCTOR,
      leaseActionType,
      FieldNames.LeaseRecipient -> addressOrAliasType,
      FieldNames.LeaseAmount    -> LONG
    ) { args =>
      val typedArgs = (leaseActionType.fields.map(_._1) zip args ::: List(CONST_LONG(0))).toMap
      Right(CaseObj(leaseActionType, typedArgs))
    }

  val detailedLeaseActionConstructor: BaseFunction[Environment] =
    NativeFunction(
      "Lease",
      1,
      DETAILED_LEASE_ACTION_CONSTRUCTOR,
      leaseActionType,
      FieldNames.LeaseRecipient -> addressOrAliasType,
      FieldNames.LeaseAmount    -> LONG,
      FieldNames.LeaseNonce     -> LONG
    ) { args =>
      val typedArgs = (leaseActionType.fields.map(_._1) zip args).toMap
      Right(CaseObj(leaseActionType, typedArgs))
    }

  val calculateLeaseId: BaseFunction[Environment] =
    NativeFunction.withEnvironment[Environment](
      "calculateLeaseId",
      1,
      CALCULATE_LEASE_ID,
      BYTESTR,
      ("lease", leaseActionType)
    ) {
      val AddressLength  = 26
      val MaxAliasLength = 30
      new ContextfulNativeFunction.Simple[Environment]("calculateLeaseId", BYTESTR, Seq(("lease", leaseActionType))) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case CaseObj(`leaseActionType`, fields) :: Nil =>
              val recipient = caseObjToRecipient(fields(FieldNames.LeaseRecipient).asInstanceOf[CaseObj])
              val r = recipient match {
                case Recipient.Address(bytes) if bytes.arr.length > AddressLength =>
                  Left(CommonError(s"Address bytes length=${bytes.arr.length} exceeds limit=$AddressLength"): ExecutionError)
                case Recipient.Alias(name) if name.length > MaxAliasLength =>
                  Left(CommonError(s"Alias name length=${name.length} exceeds limit=$MaxAliasLength"): ExecutionError)
                case _ =>
                  CONST_BYTESTR(
                    Lease.calculateId(
                      Lease(
                        recipient,
                        fields(FieldNames.LeaseAmount).asInstanceOf[CONST_LONG].t,
                        fields(FieldNames.LeaseNonce).asInstanceOf[CONST_LONG].t
                      ),
                      env.txId
                    )
                  )
              }
              r.pure[F]
            case xs => notImplemented[F, EVALUATED](s"calculateLeaseId(l: Lease)", xs)
          }
      }
    }

  def accountScriptHashF(global: BaseGlobal): BaseFunction[Environment] = {
    val name    = "scriptHash"
    val resType = UNION(BYTESTR, UNIT)
    val arg     = ("account", addressOrAliasType)
    NativeFunction.withEnvironment[Environment](
      name,
      200,
      ACCOUNTSCRIPTHASH,
      resType,
      arg
    ) {
      new ContextfulNativeFunction.Simple[Environment](
        name,
        resType,
        Seq(arg)
      ) {
        override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
          args match {
            case List(addr: CaseObj) =>
              env
                .accountScript(caseObjToRecipient(addr))
                .map(
                  _.map(si => CONST_BYTESTR(ByteStr(global.blake2b256(si.bytes().arr))))
                    .getOrElse(Right(unit))
                )

            case xs =>
              notImplemented[F, EVALUATED](s"scriptHash(account: AddressOrAlias))", xs)
          }
      }
    }
  }

}

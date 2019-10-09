package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.{Eval, Monad}
import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{BYTESTR, LONG, STRING, _}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext, _}
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.traits.domain.{OrdType, Recipient}
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}

object WavesContext {

  import Bindings._
  import Types._
  import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters._

  lazy val writeSetType = CASETYPEREF(FieldNames.WriteSet, List(FieldNames.Data -> LIST(dataEntryType)))
  val scriptTransfer =
    CASETYPEREF(FieldNames.ScriptTransfer, List("recipient" -> addressOrAliasType, "amount" -> LONG, "asset" -> optionByteVector))
  lazy val scriptTransferSetType = CASETYPEREF(FieldNames.TransferSet, List(FieldNames.Transfers -> LIST(scriptTransfer)))
  lazy val scriptResultType =
    CASETYPEREF(FieldNames.ScriptResult, List(FieldNames.ScriptWriteSet -> writeSetType, FieldNames.ScriptTransferSet -> scriptTransferSetType))

  def build[F[_] : Monad](ds: DirectiveSet, env: Environment[F]): CTX[F, Environment[F]] = {

    val version = ds.stdLibVersion
    val isTokenContext = ds.scriptType match {
      case Account => false
      case Asset   => true
    }
    val environmentFunctions = new EnvironmentFunctions[F](env)

    val proofsEnabled = !isTokenContext

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

    def getDataFromArrayF(name: String, internalName: Short, dataType: DataType): BaseFunction[F] =
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
          result.pure[F]
        case xs => notImplemented[F](s"$name(s: String)", xs)
      }

    val getIntegerFromArrayF: BaseFunction[F] = getDataFromArrayF("getInteger", DATA_LONG_FROM_ARRAY, DataType.Long)
    val getBooleanFromArrayF: BaseFunction[F] = getDataFromArrayF("getBoolean", DATA_BOOLEAN_FROM_ARRAY, DataType.Boolean)
    val getBinaryFromArrayF: BaseFunction[F]  = getDataFromArrayF("getBinary", DATA_BYTES_FROM_ARRAY, DataType.ByteArray)
    val getStringFromArrayF: BaseFunction[F]  = getDataFromArrayF("getString", DATA_STRING_FROM_ARRAY, DataType.String)

    def getDataByIndexF(name: String, dataType: DataType): BaseFunction[F] =
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

    val getIntegerByIndexF: BaseFunction[F] = getDataByIndexF("getInteger", DataType.Long)
    val getBooleanByIndexF: BaseFunction[F] = getDataByIndexF("getBoolean", DataType.Boolean)
    val getBinaryByIndexF: BaseFunction[F]  = getDataByIndexF("getBinary", DataType.ByteArray)
    val getStringByIndexF: BaseFunction[F]  = getDataByIndexF("getString", DataType.String)

    def withExtract(f: BaseFunction[F]): BaseFunction[F] = {
      val args = f.signature.args.zip(f.args).map {
        case ((name, ty), _) => ("@" ++ name, ty)
      }
      UserFunction(
        f.name ++ "Value",
        "@extr" ++ f.header.toString,
        f.costByLibVersion,
        f.signature.result.asInstanceOf[UNION].typeList.find(_ != UNIT).get,
        args: _*
      ) {
        FUNCTION_CALL(PureContext.extract, List(FUNCTION_CALL(f.header, args.map(a => REF(a._1)).toList)))
      }
    }

    def secureHashExpr(xs: EXPR): EXPR = FUNCTION_CALL(
      FunctionHeader.Native(KECCAK256),
      List(
        FUNCTION_CALL(
          FunctionHeader.Native(BLAKE256),
          List(xs)
        )
      )
    )

    lazy val addressFromPublicKeyF: BaseFunction[F] =
      UserFunction("addressFromPublicKey", 82, addressType, ("@publicKey", BYTESTR)) {

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

    def removePrefixExpr(str: EXPR, prefix: String): EXPR = IF(
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

    def verifyAddressChecksumExpr(addressBytes: EXPR): EXPR = FUNCTION_CALL(
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

    lazy val addressFromStringF: BaseFunction[F] =
      UserFunction("addressFromString", 124, optionAddress, ("@string", STRING)) {

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

    val addressFromRecipientF: BaseFunction[F] =
      NativeFunction(
        "addressFromRecipient",
        100,
        ADDRESSFROMRECIPIENT,
        addressType,
        ("AddressOrAlias", addressOrAliasType)
      ) {
        case (c @ CaseObj(`addressType`, _)) :: Nil => (c: EVALUATED).asRight[ExecutionError].pure[F]
        case CaseObj(`aliasType`, fields) :: Nil =>
          environmentFunctions
            .addressFromAlias(fields("alias").asInstanceOf[CONST_STRING].s)
            .map(_.map(resolved => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(resolved.bytes).explicitGet()))))
        case xs => notImplemented[F](s"addressFromRecipient(a: AddressOrAlias)", xs)
      }

    val stringFromAddressF: BaseFunction[F] =
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
            .pure[F]
        case xs => notImplemented[F](s"toString(a: Address)", xs)
      }

    val inputEntityCoeval: Eval[Either[String, EVALUATED]] = {
      Eval.later(
        env.inputEntity
          .eliminate(
            tx => transactionObject(tx, proofsEnabled, version).asRight[String],
            _.eliminate(
              o => orderObject(o, proofsEnabled, version).asRight[String],
              _.eliminate(
                o => Bindings.scriptTransfer(o).asRight[String],
                _ => "Expected Transaction or Order".asLeft[EVALUATED]
              )
            )
          ))
    }

    val heightCoeval: Eval[F[Either[String, EVALUATED]]] =
      Eval.later {
        env.height
          .map(v => (CONST_LONG(v): EVALUATED))
          .map(_.asRight[ExecutionError])
      }

    val accountThisCoeval: Eval[F[Either[String, EVALUATED]]] =
      Eval.later {
        (Bindings.senderObject(env.tthis): EVALUATED)
          .asRight[ExecutionError]
          .pure[F]
      }

    val assetThisCoeval: Eval[F[Either[String, EVALUATED]]] =
      Eval.later {
        env.assetInfoById(env.tthis.bytes)
          .map(v => buildAssetInfo(v.get): EVALUATED)
          .map(_.asRight[ExecutionError])
      }

    val lastBlockCoeval: Eval[F[Either[String, EVALUATED]]] =
      Eval.later {
        env.lastBlockOpt
          .map(v => Bindings.buildLastBlockInfo(v.get): EVALUATED)
          .map(_.asRight[ExecutionError])
      }

    val anyTransactionType =
      UNION(
        (buildObsoleteTransactionTypes(proofsEnabled) ++
          buildActiveTransactionTypes(proofsEnabled, version)))

    val txByIdF: BaseFunction[F] = {
      val returnType = com.wavesplatform.lang.v1.compiler.Types.UNION.create(UNIT +: anyTransactionType.typeList)
      NativeFunction("transactionById",
                     100,
                     GETTRANSACTIONBYID,
                     returnType,
                     ("id", BYTESTR)) {
        case CONST_BYTESTR(id: ByteStr) :: Nil =>
          env.transactionById(id.arr)
            .map(_.map(transactionObject(_, proofsEnabled, version)))
            .map(fromOptionCO)
            .map(_.asRight[String])
        case xs => notImplemented[F](s"transactionById(u: ByteVector)", xs)
      }
    }

    val transferTxByIdF: BaseFunction[F] =
      NativeFunction(
        "transferTransactionById",
        100,
        TRANSFERTRANSACTIONBYID,
        UNION(buildTransferTransactionType(proofsEnabled), UNIT),
        ("id", BYTESTR)
      ) {
        case CONST_BYTESTR(id: ByteStr) :: Nil =>
          env.transferTransactionById(id.arr)
            .map(_.map(transactionObject(_, proofsEnabled, version)))
            .map(fromOptionCO)
            .map(_.asRight[String])

        case xs => notImplemented[F](s"transferTransactionById(u: ByteVector)", xs)
      }

    def caseObjToRecipient(c: CaseObj): Recipient = c.caseType.name match {
      case addressType.name => Recipient.Address(c.fields("bytes").asInstanceOf[CONST_BYTESTR].bs)
      case aliasType.name   => Recipient.Alias(c.fields("alias").asInstanceOf[CONST_STRING].s)
      case _                => ???
    }

    val assetBalanceF: BaseFunction[F] =
      NativeFunction(
        "assetBalance",
        100,
        ACCOUNTASSETBALANCE,
        LONG,
        ("addressOrAlias", addressOrAliasType),
        ("assetId", UNION(UNIT, BYTESTR))
      ) {
        case (c: CaseObj) :: u :: Nil if u == unit => env.accountBalanceOf(caseObjToRecipient(c), None).map(_.map(CONST_LONG))
        case (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil =>
          env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(_.map(CONST_LONG))

        case xs => notImplemented[F](s"assetBalance(u: ByteVector|Unit)", xs)
      }

    val assetInfoF: BaseFunction[F] =
      NativeFunction(
        "assetInfo",
        100,
        GETASSETINFOBYID,
        optionAsset,
        ("id", BYTESTR)
      ) {
        case CONST_BYTESTR(id: ByteStr) :: Nil =>
          env.assetInfoById(id.arr).map(_.map(buildAssetInfo) match {
            case Some(result) => result.asRight[String]
            case _            => unit.asRight[String]
          })
        case xs => notImplemented[F](s"assetInfo(u: ByteVector)", xs)
      }

    val wavesBalanceF: UserFunction[F] =
      UserFunction("wavesBalance", 109, LONG, ("@addressOrAlias", addressOrAliasType)) {
        FUNCTION_CALL(assetBalanceF.header, List(REF("@addressOrAlias"), REF("unit")))
      }

    val txHeightByIdF: BaseFunction[F] = NativeFunction(
      "transactionHeightById",
      100,
      TRANSACTIONHEIGHTBYID,
      optionLong,
      ("id", BYTESTR)
    ) {
      case CONST_BYTESTR(id: ByteStr) :: Nil =>
        env.transactionHeightById(id.arr)
          .map(fromOptionL)
          .map(_.asRight[String])
      case xs => notImplemented[F](s"transactionHeightById(u: ByteVector)", xs)
    }

    val blockInfoByHeightF: BaseFunction[F] = NativeFunction(
      "blockInfoByHeight",
      100,
      BLOCKINFOBYHEIGHT,
      UNION(UNIT, blockInfo),
      ("height", LONG)
    ) {
      case CONST_LONG(height: Long) :: Nil =>
        env.blockInfoByHeight(height.toInt)
          .map(v => fromOptionCO(v.map(Bindings.buildLastBlockInfo)))
          .map(_.asRight[ExecutionError])
      case xs => notImplemented[F](s"blockInfoByHeight(u: Int)", xs)
    }

    val sellOrdTypeCoeval: Eval[Either[String, EVALUATED]]  = Eval.always(Right(ordType(OrdType.Sell)))
    val buyOrdTypeCoeval:  Eval[Either[String, EVALUATED]]  = Eval.always(Right(ordType(OrdType.Buy)))

    val scriptInputType =
      if (isTokenContext)
        UNION(buildAssetSupportedTransactions(proofsEnabled, version))
      else
        UNION((buildOrderType(proofsEnabled) :: buildActiveTransactionTypes(proofsEnabled, version)))

    val commonVars = Map(
      ("height", (LONG, LazyVal.fromEval(heightCoeval)))
    )

    val txVar =
      ("tx", (scriptInputType, LazyVal.fromEval(inputEntityCoeval.map(_.pure[F]))))

    lazy val accountThisVar = ("this", (addressType, LazyVal.fromEval((accountThisCoeval))))
    lazy val assetThisVar   = ("this", (assetType,   LazyVal.fromEval((assetThisCoeval))))
    lazy val thisVar = ds.scriptType match {
      case Account => accountThisVar
      case Asset   => assetThisVar
    }

    val vars = Map(
      1 -> Map(txVar),
      2 -> Map(
        ("Sell", (ordTypeType, LazyVal.fromEval((sellOrdTypeCoeval.map(_.pure[F]))))),
        ("Buy", (ordTypeType, LazyVal.fromEval((buyOrdTypeCoeval.map(_.pure[F]))))),
        txVar
      ),
      3 -> {
        val v3Part1: Map[String, (FINAL, LazyVal[F])] = Map(
          ("Sell", (sellType, LazyVal.fromEval((sellOrdTypeCoeval.map(_.pure[F]))))),
          ("Buy", (buyType, LazyVal.fromEval((buyOrdTypeCoeval.map(_.pure[F]))))),
          ("lastBlock", (blockInfo, LazyVal.fromEval((lastBlockCoeval))))
        )
        val v3Part2: Map[String, (FINAL, LazyVal[F])] = if (ds.contentType == Expression) Map(txVar, thisVar) else Map(thisVar)
        (v3Part1 ++ v3Part2)
      }
    )

    lazy val functions = Array(
      txHeightByIdF,
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
      addressFromPublicKeyF,
      addressFromStringF,
      addressFromRecipientF,
      assetBalanceF,
      wavesBalanceF
    )

    val types = buildWavesTypes(proofsEnabled, version)

    CTX(
      types ++ (if (version == V3) {
                  List(writeSetType, paymentType, scriptTransfer, scriptTransferSetType, scriptResultType, invocationType, assetType, blockInfo)
                } else List.empty),
      commonVars ++ vars(version.id),
      functions ++ (
        version match {
          case V1 | V2 => List(txByIdF)
          case V3 =>
            List(
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
            ).map(withExtract) ::: List(assetInfoF, blockInfoByHeightF, transferTxByIdF, stringFromAddressF)
        }
      )
    )
  }

  val verifierInput = UNION.create((buildOrderType(true) :: buildActiveTransactionTypes(true, V3)), Some("VerifierInput"))
}

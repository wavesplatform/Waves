package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.Eval
import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
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

  def build(ds: DirectiveSet, env: Environment): CTX = {

    val version = ds.stdLibVersion
    val isTokenContext = ds.scriptType match {
      case Account => false
      case Asset   => true
    }
    val environmentFunctions = new EnvironmentFunctions(env)

    val proofsEnabled = !isTokenContext

    def getDataFromStateF(name: String, internalName: Short, dataType: DataType): BaseFunction =
      NativeFunction(
        name,
        100,
        internalName,
        UNION(dataType.innerType, UNIT),
        ("addressOrAlias", addressOrAliasType),
        ("key", STRING)
      ) {
        case (addressOrAlias: CaseObj) :: CONST_STRING(k) :: Nil =>
          environmentFunctions.getData(addressOrAlias, k, dataType).flatMap {
            case None => Right(unit)
            case Some(a) =>
              a match {
                case b: ByteStr => CONST_BYTESTR(b)
                case b: Long    => Right(CONST_LONG(b))
                case b: String  => CONST_STRING(b)
                case b: Boolean => Right(CONST_BOOLEAN(b))
              }
          }
        case xs => notImplemented(s"$name(s: String)", xs)
      }

    val getIntegerFromStateF: BaseFunction = getDataFromStateF("getInteger", DATA_LONG_FROM_STATE, DataType.Long)
    val getBooleanFromStateF: BaseFunction = getDataFromStateF("getBoolean", DATA_BOOLEAN_FROM_STATE, DataType.Boolean)
    val getBinaryFromStateF: BaseFunction  = getDataFromStateF("getBinary", DATA_BYTES_FROM_STATE, DataType.ByteArray)
    val getStringFromStateF: BaseFunction  = getDataFromStateF("getString", DATA_STRING_FROM_STATE, DataType.String)

    def getDataFromArrayF(name: String, internalName: Short, dataType: DataType): BaseFunction =
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
          entryValue match {
            case Some(n: CONST_LONG) if dataType == DataType.Long         => Right(n)
            case Some(b: CONST_BOOLEAN) if dataType == DataType.Boolean   => Right(b)
            case Some(b: CONST_BYTESTR) if dataType == DataType.ByteArray => Right(b)
            case Some(s: CONST_STRING) if dataType == DataType.String     => Right(s)
            case _                                                        => Right(unit)
          }
        case xs => notImplemented(s"$name(s: String)", xs)
      }

    val getIntegerFromArrayF: BaseFunction = getDataFromArrayF("getInteger", DATA_LONG_FROM_ARRAY, DataType.Long)
    val getBooleanFromArrayF: BaseFunction = getDataFromArrayF("getBoolean", DATA_BOOLEAN_FROM_ARRAY, DataType.Boolean)
    val getBinaryFromArrayF: BaseFunction  = getDataFromArrayF("getBinary", DATA_BYTES_FROM_ARRAY, DataType.ByteArray)
    val getStringFromArrayF: BaseFunction  = getDataFromArrayF("getString", DATA_STRING_FROM_ARRAY, DataType.String)

    def getDataByIndexF(name: String, dataType: DataType): BaseFunction =
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

    val getIntegerByIndexF: BaseFunction = getDataByIndexF("getInteger", DataType.Long)
    val getBooleanByIndexF: BaseFunction = getDataByIndexF("getBoolean", DataType.Boolean)
    val getBinaryByIndexF: BaseFunction  = getDataByIndexF("getBinary", DataType.ByteArray)
    val getStringByIndexF: BaseFunction  = getDataByIndexF("getString", DataType.String)

    def withExtract(f: BaseFunction) = {
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

    lazy val addressFromPublicKeyF: BaseFunction =
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

    lazy val addressFromStringF: BaseFunction =
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

    val addressFromRecipientF: BaseFunction =
      NativeFunction(
        "addressFromRecipient",
        100,
        ADDRESSFROMRECIPIENT,
        addressType,
        ("AddressOrAlias", addressOrAliasType)
      ) {
        case (c @ CaseObj(`addressType`, _)) :: Nil => Right(c)
        case CaseObj(`aliasType`, fields) :: Nil =>
          environmentFunctions
            .addressFromAlias(fields("alias").asInstanceOf[CONST_STRING].s)
            .map(resolved => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(resolved.bytes).explicitGet())))
        case xs => notImplemented(s"addressFromRecipient(a: AddressOrAlias)", xs)
      }

    val stringFromAddressF: BaseFunction =
      NativeFunction(
        "toString",
        10,
        ADDRESSTOSTRING,
        STRING,
        ("Address", addressType)
      ) {
        case CaseObj(`addressType`, fields) :: Nil => CONST_STRING(fields("bytes").asInstanceOf[CONST_BYTESTR].bs.toString)
        case xs => notImplemented(s"toString(a: Address)", xs)
      }

    val inputEntityCoeval: Eval[Either[String, CaseObj]] = {
      Eval.later(
        env.inputEntity
          .eliminate(
            tx => transactionObject(tx, proofsEnabled, version).asRight[String],
            _.eliminate(
              o => orderObject(o, proofsEnabled, version).asRight[String],
              _.eliminate(
                o => Bindings.scriptTransfer(o).asRight[String],
                _ => "Expected Transaction or Order".asLeft[CaseObj]
              )
            )
          ))
    }

    val heightCoeval: Eval[Either[String, CONST_LONG]]   = Eval.later(Right(CONST_LONG(env.height)))
    val accountThisCoeval: Eval[Either[String, CaseObj]] = Eval.later(Right(Bindings.senderObject(env.tthis)))
    val assetThisCoeval: Eval[Either[String, CaseObj]]   = Eval.later(Right(buildAssetInfo(env.assetInfoById(env.tthis.bytes).get)))
    val lastBlockCoeval: Eval[Either[String, CaseObj]]   = Eval.later(Right(Bindings.buildLastBlockInfo(env.lastBlockOpt().get)))

    val anyTransactionType =
      UNION(
        (buildObsoleteTransactionTypes(proofsEnabled) ++
          buildActiveTransactionTypes(proofsEnabled, version)))

    val txByIdF: BaseFunction = {
      val returnType = com.wavesplatform.lang.v1.compiler.Types.UNION.create(UNIT +: anyTransactionType.typeList)
      NativeFunction("transactionById", 100, GETTRANSACTIONBYID, returnType,  ("id", BYTESTR)) {
        case CONST_BYTESTR(id: ByteStr) :: Nil =>
          val maybeDomainTx: Option[CaseObj] = env.transactionById(id.arr).map(transactionObject(_, proofsEnabled, version))
          Right(fromOptionCO(maybeDomainTx))
        case xs => notImplemented(s"transactionById(u: ByteVector)", xs)
      }
    }

    val transferTxByIdF: BaseFunction =
      NativeFunction(
        "transferTransactionById",
        100,
        TRANSFERTRANSACTIONBYID,
        UNION(buildTransferTransactionType(proofsEnabled), UNIT),
        ("id", BYTESTR)
      ) {
        case CONST_BYTESTR(id: ByteStr) :: Nil =>
          val transferTxO = env.transferTransactionById(id.arr).map(transactionObject(_, proofsEnabled, version))
          Right(fromOptionCO(transferTxO))

        case xs => notImplemented(s"transferTransactionById(u: ByteVector)", xs)
      }

    def caseObjToRecipient(c: CaseObj): Recipient = c.caseType.name match {
      case addressType.name => Recipient.Address(c.fields("bytes").asInstanceOf[CONST_BYTESTR].bs)
      case aliasType.name   => Recipient.Alias(c.fields("alias").asInstanceOf[CONST_STRING].s)
      case _                => ???
    }

    val assetBalanceF: BaseFunction =
      NativeFunction(
        "assetBalance",
        100,
        ACCOUNTASSETBALANCE,
        LONG,
        ("addressOrAlias", addressOrAliasType),
        ("assetId", UNION(UNIT, BYTESTR))
      ) {
        case (c: CaseObj) :: u :: Nil if u == unit => env.accountBalanceOf(caseObjToRecipient(c), None).map(CONST_LONG)
        case (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil =>
          env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(CONST_LONG)

        case xs => notImplemented(s"assetBalance(u: ByteVector|Unit)", xs)
      }

    val assetInfoF: BaseFunction =
      NativeFunction(
        "assetInfo",
        100,
        GETASSETINFOBYID,
        optionAsset,
        ("id", BYTESTR)
      ) {
        case CONST_BYTESTR(id: ByteStr) :: Nil =>
          env.assetInfoById(id.arr).map(buildAssetInfo(_)) match {
            case Some(result) => Right(result)
            case _            => Right(unit)
          }
        case xs => notImplemented(s"assetInfo(u: ByteVector)", xs)
      }

    val wavesBalanceF: UserFunction =
      UserFunction("wavesBalance", 109, LONG, ("@addressOrAlias", addressOrAliasType)) {
        FUNCTION_CALL(assetBalanceF.header, List(REF("@addressOrAlias"), REF("unit")))

      }

    val txHeightByIdF: BaseFunction = NativeFunction(
      "transactionHeightById",
      100,
      TRANSACTIONHEIGHTBYID,
      optionLong,
      ("id", BYTESTR)
    ) {
      case CONST_BYTESTR(id: ByteStr) :: Nil => Right(fromOptionL(env.transactionHeightById(id.arr).map(_.toLong)))
      case xs => notImplemented(s"transactionHeightById(u: ByteVector)", xs)
    }

    val blockInfoByHeightF: BaseFunction = NativeFunction(
      "blockInfoByHeight",
      100,
      BLOCKINFOBYHEIGHT,
      UNION(UNIT, blockInfo),
      ("height", LONG)
    ) {
      case CONST_LONG(height: Long) :: Nil => Right(env.blockInfoByHeight(height.toInt).map(Bindings.buildLastBlockInfo))
      case xs => notImplemented(s"blockInfoByHeight(u: Int)", xs)
    }

    val parseBlockHeaderF: BaseFunction =
      NativeFunction(
        "parseBlockHeader",
        100,
        BLOCKHEADER_FROM_BYTES,
        UNION.create(UNIT :: blockHeader :: Nil),
        ("blockHeaderBytes", BYTESTR)
      ) {
        case CONST_BYTESTR(headerBytes) :: Nil =>
          val maybeHeaderObj =
            env
              .blockHeaderParser(headerBytes)
              .map(Bindings.blockHeaderObject)

          fromOptionCO(maybeHeaderObj).asRight[String]

        case _ => ???
      }

    val sellOrdTypeCoeval: Eval[Either[String, CaseObj]] = Eval.always(Right(ordType(OrdType.Sell)))
    val buyOrdTypeCoeval: Eval[Either[String, CaseObj]]  = Eval.always(Right(ordType(OrdType.Buy)))

    val scriptInputType =
      if (isTokenContext)
        UNION(buildAssetSupportedTransactions(proofsEnabled, version))
      else
        UNION((buildOrderType(proofsEnabled) :: buildActiveTransactionTypes(proofsEnabled, version)))

    val commonVars = Map(
      ("height", (com.wavesplatform.lang.v1.compiler.Types.LONG, LazyVal(EitherT(heightCoeval)))),
    )

    val txVar = ("tx", (scriptInputType, LazyVal(EitherT(inputEntityCoeval))))

    lazy val accountThisVar = ("this", (addressType, LazyVal(EitherT(accountThisCoeval))))
    lazy val assetThisVar   = ("this", (assetType,  LazyVal(EitherT(assetThisCoeval))))
    lazy val thisVar = ds.scriptType match {
      case Account => accountThisVar
      case Asset   => assetThisVar
    }

    val v1vars = Map(txVar)
    val v2vars = v1vars ++ Map(
      ("Sell", (ordTypeType, LazyVal(EitherT(sellOrdTypeCoeval)))),
      ("Buy", (ordTypeType, LazyVal(EitherT(buyOrdTypeCoeval)))),
    )
    val v3vars = {
      val v3Part1: Map[String, (FINAL, LazyVal)] = Map(
        ("Sell", (sellType, LazyVal(EitherT(sellOrdTypeCoeval)))),
        ("Buy", (buyType, LazyVal(EitherT(buyOrdTypeCoeval)))),
        ("lastBlock", (blockInfo, LazyVal(EitherT(lastBlockCoeval))))
      )
      val v3Part2: Map[String, (FINAL, LazyVal)] = if (ds.contentType == Expression) Map(txVar, thisVar) else Map(thisVar)
      (v3Part1 ++ v3Part2)
    }

    val vars = Map(
      V1 -> v1vars,
      V2 -> v2vars,
      V3 -> v3vars,
      V4 -> v3vars
    )

    lazy val commonFunctions = Array(
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

    lazy val v3Functions: List[BaseFunction] = List(
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

    lazy val v4Functions: List[BaseFunction] = List(
      parseBlockHeaderF
    )

    val functions = Map[StdLibVersion, List[BaseFunction]](
      V1 -> List(txByIdF),
      V2 -> List(txByIdF),
      V3 -> v3Functions,
      V4 -> (v3Functions ++ v4Functions)
    )

    val commonTypes: List[FINAL] = buildWavesTypes(proofsEnabled, version).toList
    val v3types: List[CASETYPEREF] = List(
      writeSetType,
      paymentType,
      scriptTransfer,
      scriptTransferSetType,
      scriptResultType,
      invocationType,
      assetType,
      blockInfo
    )
    val v4types: List[CASETYPEREF] = List(blockHeader)

    val types = Map[StdLibVersion, List[CASETYPEREF]](
      V1 -> List.empty,
      V2 -> List.empty,
      V3 -> v3types,
      V4 -> (v3types ++ v4types),
    )

    CTX(
      commonTypes ++ types(version),
      commonVars ++ vars(version),
      commonFunctions ++ functions(version)
    )
  }

  val verifierInput = UNION.create((buildOrderType(true) :: buildActiveTransactionTypes(true, V3)), Some("VerifierInput"))

}

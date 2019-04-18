package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
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
import monix.eval.Coeval

object WavesContext {

  import Bindings._
  import Types._
  import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters._

  lazy val writeSetType = CASETYPEREF(FieldNames.WriteSet, List(FieldNames.Data -> LIST(dataEntryType)))
  val scriptTransfer =
    CASETYPEREF(FieldNames.ScriptTransfer, List("recipient" -> addressOrAliasType, "amount" -> LONG, "asset" -> optionByteVector))
  lazy val scriptTransferSetType = CASETYPEREF(FieldNames.TransferSet, List(FieldNames.Transfers -> LIST(scriptTransfer)))
  lazy val scriptResultType =
    CASETYPEREF(FieldNames.ScriptResult, List(FieldNames.Data -> writeSetType, FieldNames.Transfers -> scriptTransferSetType))

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
        "get data from the account state",
        ("addressOrAlias", addressOrAliasType, "account"),
        ("key", STRING, "key")
      ) {
        case (addressOrAlias: CaseObj) :: CONST_STRING(k) :: Nil =>
          environmentFunctions.getData(addressOrAlias, k, dataType).map {
            case None => unit
            case Some(a) =>
              a match {
                case b: ByteStr => CONST_BYTESTR(b)
                case b: Long    => CONST_LONG(b)
                case b: String  => CONST_STRING(b)
                case b: Boolean => CONST_BOOLEAN(b)
              }
          }
        case _ => ???
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
        "Find and extract data by key",
        ("data", LIST(dataEntryType), "DataEntry vector, usally tx.data"),
        ("key", STRING, "key")
      ) {
        case ARR(data: IndexedSeq[CaseObj] @unchecked) :: CONST_STRING(key: String) :: Nil =>
          data.find(_.fields("key") == CONST_STRING(key)).map(_.fields("value")) match {
            case Some(n: CONST_LONG) if dataType == DataType.Long         => Right(n)
            case Some(b: CONST_BOOLEAN) if dataType == DataType.Boolean   => Right(b)
            case Some(b: CONST_BYTESTR) if dataType == DataType.ByteArray => Right(b)
            case Some(s: CONST_STRING) if dataType == DataType.String     => Right(s)
            case _                                                        => Right(unit)
          }
        case _ => ???
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
        "Extract data by index",
        ("@data", LIST(dataEntryType), "DataEntry vector, usally tx.data"),
        ("@index", LONG, "index")
      ) {
        LET_BLOCK(
          LET("@val", GETTER(FUNCTION_CALL(PureContext.getElement, List(REF("@data"), REF("@index"))), "value")),
          IF(FUNCTION_CALL(PureContext._isInstanceOf, List(REF("@val"), CONST_STRING(dataType.innerType.name))), REF("@val"), REF("unit"))
        )
      }

    val getIntegerByIndexF: BaseFunction = getDataByIndexF("getInteger", DataType.Long)
    val getBooleanByIndexF: BaseFunction = getDataByIndexF("getBoolean", DataType.Boolean)
    val getBinaryByIndexF: BaseFunction  = getDataByIndexF("getBinary", DataType.ByteArray)
    val getStringByIndexF: BaseFunction  = getDataByIndexF("getString", DataType.String)

    def withExtract(f: BaseFunction) = {
      val args = f.signature.args.zip(f.argsDoc).map {
        case ((name, ty), (_name, doc)) => ("@" ++ name, ty, doc)
      }
      UserFunction(
        f.name ++ "Value",
        "@extr" ++ f.header.toString,
        f.costByLibVersion,
        f.signature.result.asInstanceOf[UNION].typeList.find(_ != UNIT).get,
        f.docString ++ " (fail on error)",
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
      UserFunction("addressFromPublicKey", 82, addressType, "Convert public key to account address", ("@publicKey", BYTESTR, "public key")) {

        FUNCTION_CALL(
          FunctionHeader.User("Address"),
          List(
            LET_BLOCK(
              LET(
                "@afpk_withoutChecksum",
                FUNCTION_CALL(
                  PureContext.sumByteStr,
                  List(
                    CONST_BYTESTR(ByteStr.fromBytes(EnvironmentFunctions.AddressVersion, env.chainId)),
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
          CONST_STRING(prefix)
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
      UserFunction("addressFromString", 124, optionAddress, "Decode account address", ("@string", STRING, "string address represntation")) {

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
                  CONST_BYTESTR(ByteStr.fromBytes(EnvironmentFunctions.AddressVersion))
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
                    CONST_BYTESTR(ByteStr.fromBytes(env.chainId))
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
        "Extract address or lookup alias",
        ("AddressOrAlias", addressOrAliasType, "address or alias, usually tx.recipient")
      ) {
        case (c @ CaseObj(`addressType`, _)) :: Nil => Right(c)
        case CaseObj(`aliasType`, fields) :: Nil =>
          environmentFunctions
            .addressFromAlias(fields("alias").asInstanceOf[CONST_STRING].s)
            .map(resolved => CaseObj(addressType, Map("bytes" -> CONST_BYTESTR(resolved.bytes))))
        case _ => ???
      }

    val inputEntityCoeval: Coeval[Either[String, CaseObj]] = {
      Coeval.evalOnce(
        env.inputEntity
          .eliminate(
            tx => transactionObject(tx, proofsEnabled).asRight[String],
            _.eliminate(
              o => orderObject(o, proofsEnabled).asRight[String],
              _.eliminate(
                o => Bindings.scriptTransfer(o).asRight[String],
                _ => "Expected Transaction or Order".asLeft[CaseObj]
              )
            )
          ))
    }

    val heightCoeval: Coeval[Either[String, CONST_LONG]] = Coeval.evalOnce(Right(CONST_LONG(env.height)))
    val thisCoeval: Coeval[Either[String, CaseObj]]      = Coeval.evalOnce(Right(Bindings.senderObject(env.tthis)))

    val anyTransactionType =
      UNION(
        (buildObsoleteTransactionTypes(proofsEnabled) ++
          buildActiveTransactionTypes(proofsEnabled, version)))

    val txByIdF: BaseFunction = {
      val returnType = com.wavesplatform.lang.v1.compiler.Types.UNION.create(UNIT +: anyTransactionType.typeList)
      NativeFunction(
        "transactionById",
        Map[StdLibVersion, Long](V1 -> 100, V2 -> 100, V3 -> 500, V4 -> 500),
        GETTRANSACTIONBYID,
        returnType,
        "Lookup transaction",
        ("id", BYTESTR, "transaction Id")
      ) {
        case CONST_BYTESTR(id: ByteStr) :: Nil =>
          val maybeDomainTx: Option[CaseObj] = env.transactionById(id.arr).map(transactionObject(_, proofsEnabled))
          Right(fromOptionCO(maybeDomainTx))
        case _ => ???
      }
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
        "get asset balance for account",
        ("addressOrAlias", addressOrAliasType, "account"),
        ("assetId", UNION(UNIT, BYTESTR), "assetId (WAVES if none)")
      ) {
        case (c: CaseObj) :: u :: Nil if u == unit => env.accountBalanceOf(caseObjToRecipient(c), None).map(CONST_LONG)
        case (c: CaseObj) :: CONST_BYTESTR(assetId: ByteStr) :: Nil =>
          env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.arr)).map(CONST_LONG)

        case _ => ???
      }

    val wavesBalanceF: UserFunction =
      UserFunction("wavesBalance", 109, LONG, "get WAVES balanse for account", ("@addressOrAlias", addressOrAliasType, "account")) {
        FUNCTION_CALL(assetBalanceF.header, List(REF("@addressOrAlias"), REF("unit")))

      }

    val txHeightByIdF: BaseFunction = NativeFunction(
      "transactionHeightById",
      100,
      TRANSACTIONHEIGHTBYID,
      optionLong,
      "get height when transaction was stored to blockchain",
      ("id", BYTESTR, "transaction Id")
    ) {
      case CONST_BYTESTR(id: ByteStr) :: Nil => Right(fromOptionL(env.transactionHeightById(id.arr).map(_.toLong)))
      case _                                 => ???
    }

    val sellOrdTypeCoeval: Coeval[Either[String, CaseObj]] = Coeval(Right(ordType(OrdType.Sell)))
    val buyOrdTypeCoeval: Coeval[Either[String, CaseObj]]  = Coeval(Right(ordType(OrdType.Buy)))

    val scriptInputType =
      if (isTokenContext)
        UNION(buildAssetSupportedTransactions(proofsEnabled, version))
      else
        UNION((buildOrderType(proofsEnabled) :: buildActiveTransactionTypes(proofsEnabled, version)))

    val commonVars = Map(
      ("height", ((com.wavesplatform.lang.v1.compiler.Types.LONG, "Current blockchain height"), LazyVal(EitherT(heightCoeval)))),
    )

    val txVar   = ("tx", ((scriptInputType, "Processing transaction"), LazyVal(EitherT(inputEntityCoeval))))
    val thisVar = ("this", ((addressType, "Script address"), LazyVal(EitherT(thisCoeval))))

    val vars = Map(
      1 -> Map(txVar),
      2 -> Map(
        ("Sell", ((ordTypeType, "Sell OrderType"), LazyVal(EitherT(sellOrdTypeCoeval)))),
        ("Buy", ((ordTypeType, "Buy OrderType"), LazyVal(EitherT(buyOrdTypeCoeval)))),
        txVar
      ),
      3 -> {
        val v3Part1: Map[String, ((FINAL, String), LazyVal)] = Map(
          ("Sell", ((sellType, "Sell OrderType"), LazyVal(EitherT(sellOrdTypeCoeval)))),
          ("Buy", ((buyType, "Buy OrderType"), LazyVal(EitherT(buyOrdTypeCoeval))))
        )
        val v3Part2: Map[String, ((FINAL, String), LazyVal)] = if (ds.contentType == Expression) Map(txVar, thisVar) else Map(thisVar)
        (v3Part1 ++ v3Part2)
      },
      4 -> {
        val v3Part1: Map[String, ((FINAL, String), LazyVal)] = Map(
          ("Sell", ((sellType, "Sell OrderType"), LazyVal(EitherT(sellOrdTypeCoeval)))),
          ("Buy", ((buyType, "Buy OrderType"), LazyVal(EitherT(buyOrdTypeCoeval))))
        )
        val v3Part2: Map[String, ((FINAL, String), LazyVal)] = if (ds.contentType == Expression) Map(txVar, thisVar) else Map(thisVar)
        (v3Part1 ++ v3Part2)
      }
    )

    lazy val functions = Array(
      txByIdF,
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

    val blockHeaderFromBytesF: BaseFunction =
      NativeFunction(
        "blockHeaderFromBytes",
        100,
        BLOCKHEADER_FROM_BYTES,
        UNION.create(UNIT :: blockHeader :: Nil),
        "parse block header from bytes",
        ("blockHeaderBytes", BYTESTR, "block header bytes")
      ) {
        case CONST_BYTESTR(headerBytes) :: Nil =>
          val maybeHeaderObj =
            env
              .blockHeaderParser(headerBytes)
              .map(Bindings.blockHeaderObject)

          fromOptionCO(maybeHeaderObj).asRight[String]

        case _ => ???
      }

    val transactionFromBytesF: BaseFunction =
      NativeFunction(
        "transactionFromBytes",
        100,
        TRANSACTION_FROM_BYTES,
        UNION.create(UNIT :: anyTransactionType.typeList),
        "parse transaction from bytes",
        ("transactionBytes", BYTESTR, "transaction bytes")
      ) {
        case CONST_BYTESTR(txBytes) :: Nil =>
          val maybeTxObj =
            env
              .transactionParser(txBytes)
              .map(Bindings.transactionObject(_, proofsEnabled))

          fromOptionCO(maybeTxObj).asRight[String]
      }

    val calculatePosDelayF: BaseFunction = {
      NativeFunction(
        "calculatePosDelay",
        100,
        CALCULATE_POS_DELAY,
        LONG,
        "calculate PoS delay in milliseconds",
        ("hit", BYTESTR, ""),
        ("baseTarget", LONG, "base target"),
        ("balance", LONG, "miner effective balance")
      ) {
        case CONST_BYTESTR(hit) :: CONST_LONG(baseTarget) :: CONST_LONG(balance) :: Nil =>
          val delay = env.calculatePoSDelay(hit, baseTarget, balance)
          CONST_LONG(delay).asRight[String]
        case _ => ???
      }
    }

    val accountScriptHashF: BaseFunction = {
      NativeFunction(
        "accountScriptHash",
        100,
        ACCOUNT_SCRIPT_HASH,
        UNION.create(Seq(UNIT, BYTESTR)),
        "Get hash of account script",
        ("addressOrAlias", addressOrAliasType, "miner effective balance")
      ) {
        case (c @ CaseObj(typ, _)) :: Nil if typ == addressType || typ == aliasType =>
          val maybeScriptHash = env
            .accountScriptHash(caseObjToRecipient(c))
            .map(ByteStr(_))

          fromOptionBV(maybeScriptHash).asRight[String]
        case _ => ???
      }
    }

    val v3Functions = List(
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
    )

    val v4Functions = List(
      blockHeaderFromBytesF,
      transactionFromBytesF,
      calculatePosDelayF,
      accountScriptHashF
    )

    val types = buildWavesTypes(proofsEnabled, version)

    CTX(
      types ++ (if (version == V3 || version == V4) {
                  List(writeSetType, paymentType, scriptTransfer, scriptTransferSetType, scriptResultType, invocationType)
                } else List.empty),
      commonVars ++ vars(version.id),
      functions ++
        (if (version == V3) v3Functions.map(withExtract) else List.empty) ++
        (if (version == V4) v3Functions.map(withExtract) ++ v4Functions else List.empty)
    )
  }

  val verifierInput = UNION.create((buildOrderType(true) :: buildActiveTransactionTypes(true, V3)), Some("VerifierInput"))
}

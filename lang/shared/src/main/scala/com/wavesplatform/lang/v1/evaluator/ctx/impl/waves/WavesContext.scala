package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.data.EitherT
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{BYTEVECTOR, LONG, STRING, _}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.fromOption
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext}
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  import Bindings._
  import Types._

  def build(env: Environment): CTX = {
    val environmentFunctions = new EnvironmentFunctions(env)

    def getDataFromStateF(name: String, internalName: Short, dataType: DataType): BaseFunction =
      NativeFunction(name, 100, internalName, UNION(dataType.innerType, UNIT), "get data from the account state", ("addressOrAlias", addressOrAliasType, "account"), ("key", STRING, "key")) {
        case (addressOrAlias: CaseObj) :: (k: String) :: Nil => environmentFunctions.getData(addressOrAlias, k, dataType).map(fromOption)
        case _                                               => ???
      }

    val getIntegerFromStateF: BaseFunction = getDataFromStateF("getInteger", DATA_LONG_FROM_STATE, DataType.Long)
    val getBooleanFromStateF: BaseFunction = getDataFromStateF("getBoolean", DATA_BOOLEAN_FROM_STATE, DataType.Boolean)
    val getBinaryFromStateF: BaseFunction  = getDataFromStateF("getBinary", DATA_BYTES_FROM_STATE, DataType.ByteArray)
    val getStringFromStateF: BaseFunction  = getDataFromStateF("getString", DATA_STRING_FROM_STATE, DataType.String)

    def getDataFromArrayF(name: String, internalName: Short, dataType: DataType): BaseFunction =
      NativeFunction(name, 10, internalName, UNION(dataType.innerType, UNIT), "Find and extract data by key", ("data", LIST(dataEntryType.typeRef), "DataEntry vector, usally tx.data"), ("key", STRING, "key")) {
        case (data: IndexedSeq[CaseObj] @unchecked) :: (key: String) :: Nil =>
          data.find(_.fields("key") == key).map(_.fields("value")) match {
            case Some(n: Long) if dataType == DataType.Long            => Right(n)
            case Some(b: Boolean) if dataType == DataType.Boolean      => Right(b)
            case Some(b: ByteVector) if dataType == DataType.ByteArray => Right(b)
            case Some(s: String) if dataType == DataType.String        => Right(s)
            case _                                                     => Right(())
          }
        case _ => ???
      }

    val getIntegerFromArrayF: BaseFunction = getDataFromArrayF("getInteger", DATA_LONG_FROM_ARRAY, DataType.Long)
    val getBooleanFromArrayF: BaseFunction = getDataFromArrayF("getBoolean", DATA_BOOLEAN_FROM_ARRAY, DataType.Boolean)
    val getBinaryFromArrayF: BaseFunction  = getDataFromArrayF("getBinary", DATA_BYTES_FROM_ARRAY, DataType.ByteArray)
    val getStringFromArrayF: BaseFunction  = getDataFromArrayF("getString", DATA_STRING_FROM_ARRAY, DataType.String)

    def getDataByIndexF(name: String, dataType: DataType): BaseFunction =
      UserFunction(name, UNION(dataType.innerType, UNIT), "Extract data by index", ("@data", LIST(dataEntryType.typeRef), "DataEntry vector, usally tx.data"), ("@index", LONG, "index")) {
        BLOCK(
          LET("@val", GETTER(FUNCTION_CALL(PureContext.getElement, List(REF("@data"), REF("@index"))), "value")),
          IF(FUNCTION_CALL(PureContext._isInstanceOf, List(REF("@val"), CONST_STRING(dataType.innerType.name))), REF("@val"), REF("unit"))
        )
      }

    val getIntegerByIndexF: BaseFunction = getDataByIndexF("getInteger", DataType.Long)
    val getBooleanByIndexF: BaseFunction = getDataByIndexF("getBoolean", DataType.Boolean)
    val getBinaryByIndexF: BaseFunction  = getDataByIndexF("getBinary", DataType.ByteArray)
    val getStringByIndexF: BaseFunction  = getDataByIndexF("getString", DataType.String)

    def secureHashExpr(xs: EXPR): EXPR = FUNCTION_CALL(
      FunctionHeader.Native(KECCAK256),
      List(
        FUNCTION_CALL(
          FunctionHeader.Native(BLAKE256),
          List(xs)
        )
      )
    )

    val addressFromPublicKeyF: BaseFunction = UserFunction("addressFromPublicKey", addressType.typeRef, "Convert public key to account address", ("@publicKey", BYTEVECTOR, "public key")) {
      FUNCTION_CALL(
        FunctionHeader.User("Address"),
        List(
          BLOCK(
            LET(
              "@afpk_withoutChecksum",
              FUNCTION_CALL(
                PureContext.sumByteVector,
                List(
                  CONST_BYTEVECTOR(ByteVector(EnvironmentFunctions.AddressVersion, env.networkByte)),
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
              PureContext.sumByteVector,
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

    val addressFromStringF: BaseFunction = UserFunction("addressFromString", optionAddress, "Decode account address", ("@string", STRING, "string address represntation")) {
      BLOCK(
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
                CONST_BYTEVECTOR(ByteVector(EnvironmentFunctions.AddressVersion))
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
                  CONST_BYTEVECTOR(ByteVector(env.networkByte))
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
      NativeFunction("addressFromRecipient", 100, ADDRESSFROMRECIPIENT, addressType.typeRef, "Extract address or lookup alias", ("AddressOrAlias", addressOrAliasType, "address or alias, usually tx.recipient")) {
        case (c @ CaseObj(addressType.typeRef, _)) :: Nil => Right(c)
        case CaseObj(aliasType.typeRef, fields) :: Nil =>
          environmentFunctions
            .addressFromAlias(fields("alias").asInstanceOf[String])
            .map(resolved => CaseObj(addressType.typeRef, Map("bytes" -> resolved.bytes)))
        case _ => ???
      }

    val inputEntityCoeval: Coeval[Either[String, CaseObj]] =
      Coeval.evalOnce(
        env.inputEntity
          .eliminate(
            tx => transactionObject(tx).asRight[String],
            _.eliminate(
              o => orderObject(o).asRight[String],
              _ => "Expected Transaction or Order".asLeft[CaseObj]
            )
          ))

    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val txByIdF: BaseFunction = {
      val returnType = com.wavesplatform.lang.v1.compiler.Types.UNION.create(com.wavesplatform.lang.v1.compiler.Types.UNIT +: anyTransactionType.l)
      NativeFunction("transactionById", 100, GETTRANSACTIONBYID, returnType, "Lookup transaction", ("id", BYTEVECTOR, "transaction Id")) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(fromOption(maybeDomainTx))
        case _ => ???
      }
    }

    def caseObjToRecipient(c: CaseObj): Recipient = c.caseType.name match {
      case addressType.typeRef.name => Recipient.Address(c.fields("bytes").asInstanceOf[ByteVector])
      case aliasType.typeRef.name   => Recipient.Alias(c.fields("alias").asInstanceOf[String])
      case _                        => ???
    }

    val assetBalanceF: BaseFunction =
      NativeFunction("assetBalance", 100, ACCOUNTASSETBALANCE, LONG, "get asset balance for account", ("addressOrAlias", addressOrAliasType, "account"), ("assetId", UNION(UNIT, BYTEVECTOR), "assetId (WAVES if none)")) {
        case (c: CaseObj) :: (()) :: Nil                  => env.accountBalanceOf(caseObjToRecipient(c), None)
        case (c: CaseObj) :: (assetId: ByteVector) :: Nil => env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.toArray))

        case _ => ???
      }

    val wavesBalanceF: UserFunction = UserFunction("wavesBalance", LONG, "get WAVES balanse for account", ("@addressOrAlias", addressOrAliasType, "account")) {
      FUNCTION_CALL(assetBalanceF.header, List(REF("@addressOrAlias"), REF("unit")))
    }

    val txHeightByIdF: BaseFunction = NativeFunction("transactionHeightById", 100, TRANSACTIONHEIGHTBYID, optionLong, "get height when transaction was stored to blockchain", ("id", BYTEVECTOR, "transaction Id")) {
      case (id: ByteVector) :: Nil => Right(fromOption(env.transactionHeightById(id.toArray)))
      case _                       => ???
    }

    val vars: Map[String, ((FINAL, String), LazyVal)] = Map(
      ("height", ((com.wavesplatform.lang.v1.compiler.Types.LONG, "Current blockchain height"), LazyVal(EitherT(heightCoeval)))),
      ("tx", ((scriptInputType, "Processing transaction"), LazyVal(EitherT(inputEntityCoeval))))
    )

    val functions = Array(
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

    CTX(Types.wavesTypes, vars, functions)
  }
}

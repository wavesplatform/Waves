package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.fromOption
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  import Bindings._
  import Types._

  def build(env: Environment): CTX = {
    val environmentFunctions = new EnvironmentFunctions(env)

    def getdataF(name: String, internalName: Short, dataType: DataType): BaseFunction =
      NativeFunction(name, 100, internalName, UNION(dataType.innerType, UNIT), "addressOrAlias" -> addressOrAliasType, "key" -> STRING) {
        case (addressOrAlias: CaseObj) :: (k: String) :: Nil => environmentFunctions.getData(addressOrAlias, k, dataType).map(fromOption)
        case _                                               => ???
      }

    val getIntegerF: BaseFunction = getdataF("getInteger", DATA_LONG, DataType.Long)
    val getBooleanF: BaseFunction = getdataF("getBoolean", DATA_BOOLEAN, DataType.Boolean)
    val getBinaryF: BaseFunction = getdataF("getBinary", DATA_BYTES, DataType.ByteArray)
    val getStringF: BaseFunction = getdataF("getString", DATA_STRING, DataType.String)

    def secureHashExpr(xs: EXPR): EXPR = FUNCTION_CALL(
      FunctionHeader.Native(KECCAK256),
      List(
        FUNCTION_CALL(
          FunctionHeader.Native(BLAKE256),
          List(xs)
        )
      )
    )

    val addressFromBytesF = NativeFunction("addressFromBytes", 1, ADDRESSFROMBYTES, addressType.typeRef, "bytes" -> BYTEVECTOR) {
      case (bytes: ByteVector) :: Nil => Right(CaseObj(addressType.typeRef, Map("bytes" -> bytes)))
      case _                          => ???
    }

    val addressFromPublicKeyF: BaseFunction = UserFunction("addressFromPublicKey", 100, addressType.typeRef, "publicKey" -> BYTEVECTOR) {
      case pk :: Nil =>
        Right(
          FUNCTION_CALL(
            FunctionHeader.Native(ADDRESSFROMBYTES),
            List(
              BLOCK(
                LET(
                  "@afpk_withoutChecksum",
                  FUNCTION_CALL(
                    FunctionHeader.Native(SUM_BYTES),
                    List(
                      CONST_BYTEVECTOR(ByteVector(EnvironmentFunctions.AddressVersion, env.networkByte)),
                      // publicKeyHash
                      FUNCTION_CALL(
                        FunctionHeader.Native(TAKE_BYTES),
                        List(
                          secureHashExpr(pk),
                          CONST_LONG(EnvironmentFunctions.HashLength)
                        )
                      )
                    )
                  )
                ),
                // bytes
                FUNCTION_CALL(
                  FunctionHeader.Native(SUM_BYTES),
                  List(
                    REF("@afpk_withoutChecksum"),
                    FUNCTION_CALL(
                      FunctionHeader.Native(TAKE_BYTES),
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
        )
      case _ => ???
    }

    def removePrefixExpr(str: EXPR, prefix: String): EXPR = IF(
      FUNCTION_CALL(
        FunctionHeader.Native(EQ),
        List(
          FUNCTION_CALL(FunctionHeader.Native(TAKE_STRING), List(str, CONST_LONG(prefix.length))),
          CONST_STRING(prefix)
        )
      ),
      FUNCTION_CALL(FunctionHeader.Native(DROP_STRING), List(str, CONST_LONG(prefix.length))),
      str
    )

    def verifyAddressChecksumExpr(addressBytes: EXPR): EXPR = FUNCTION_CALL(
      FunctionHeader.Native(EQ),
      List(
        // actual checksum
        FUNCTION_CALL(FunctionHeader.User("takeRightBytes"), List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength))),
        // generated checksum
        FUNCTION_CALL(
          FunctionHeader.Native(TAKE_BYTES),
          List(
            secureHashExpr(FUNCTION_CALL(FunctionHeader.User("dropRightBytes"), List(addressBytes, CONST_LONG(EnvironmentFunctions.ChecksumLength)))),
            CONST_LONG(EnvironmentFunctions.ChecksumLength)
          )
        )
      )
    )

    val addressFromStringF: BaseFunction = UserFunction("addressFromString", 100, optionAddress, "string" -> STRING) {
      case (str: EXPR) :: Nil =>
        Right(
          BLOCK(
            LET("@afs_addrBytes", FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(removePrefixExpr(str, EnvironmentFunctions.AddressPrefix)))),
            IF(
              FUNCTION_CALL(
                FunctionHeader.Native(EQ),
                List(
                  FUNCTION_CALL(FunctionHeader.Native(SIZE_BYTES), List(REF("@afs_addrBytes"))),
                  CONST_LONG(EnvironmentFunctions.AddressLength)
                )
              ),
              IF(
                // version
                FUNCTION_CALL(
                  FunctionHeader.Native(EQ),
                  List(
                    FUNCTION_CALL(FunctionHeader.Native(TAKE_BYTES), List(REF("@afs_addrBytes"), CONST_LONG(1))),
                    CONST_BYTEVECTOR(ByteVector(EnvironmentFunctions.AddressVersion))
                  )
                ),
                IF(
                  // networkByte
                  FUNCTION_CALL(
                    FunctionHeader.Native(EQ),
                    List(
                      FUNCTION_CALL(
                        FunctionHeader.Native(TAKE_BYTES),
                        List(
                          FUNCTION_CALL(FunctionHeader.Native(DROP_BYTES), List(REF("@afs_addrBytes"), CONST_LONG(1))),
                          CONST_LONG(1)
                        )
                      ),
                      CONST_BYTEVECTOR(ByteVector(env.networkByte))
                    )
                  ),
                  IF(
                    verifyAddressChecksumExpr(REF("@afs_addrBytes")),
                    FUNCTION_CALL(
                      FunctionHeader.Native(SOME),
                      List(FUNCTION_CALL(FunctionHeader.Native(ADDRESSFROMBYTES), List(REF("@afs_addrBytes"))))
                    ),
                    REF("None")
                  ),
                  REF("None")
                ),
                REF("None")
              ),
              REF("None")
            )
          )
        )
      case _ => ???
    }

    val addressFromRecipientF: BaseFunction =
      NativeFunction("addressFromRecipient", 100, ADDRESSFROMRECIPIENT, addressType.typeRef, "AddressOrAlias" -> addressOrAliasType) {
        case (c @ CaseObj(addressType.typeRef, _)) :: Nil => Right(c)
        case CaseObj(aliasType.typeRef, fields) :: Nil =>
          environmentFunctions
            .addressFromAlias(fields("alias").asInstanceOf[String])
            .map(resolved => CaseObj(addressType.typeRef, Map("bytes" -> resolved.bytes)))
        case _ => ???
      }

    val txCoeval: Coeval[Either[String, CaseObj]]  = Coeval.evalOnce(Right(transactionObject(env.transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val txByIdF: BaseFunction = {
      val returnType = UNION(anyTransactionType, UNIT)
      NativeFunction("getTransactionById", 100, GETTRANSACTIONBYID, returnType, "id" -> BYTEVECTOR) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(fromOption(maybeDomainTx))
        case _ => ???
      }
    }

    val accountBalanceF: BaseFunction = NativeFunction("accountBalance", 100, ACCOUNTBALANCE, LONG, "addressOrAlias" -> addressOrAliasType) {
      case CaseObj(_, fields) :: Nil =>
        val acc = fields("bytes").asInstanceOf[ByteVector].toArray
        env.accountBalanceOf(acc, None)

      case _ => ???
    }

    val accountAssetBalanceF: BaseFunction =
      NativeFunction("accountAssetBalance", 100, ACCOUNTASSETBALANCE, LONG, "addressOrAlias" -> addressOrAliasType, "assetId" -> BYTEVECTOR) {
        case CaseObj(_, fields) :: (assetId: ByteVector) :: Nil =>
          val acc = fields("bytes").asInstanceOf[ByteVector]
          env.accountBalanceOf(acc.toArray, Some(assetId.toArray))

        case _ => ???
      }

    val txHeightByIdF: BaseFunction = NativeFunction("transactionHeightById", 100, TRANSACTIONHEIGHTBYID, optionLong, "id" -> BYTEVECTOR) {
      case (id: ByteVector) :: Nil => Right(fromOption(env.transactionHeightById(id.toArray)))
      case _                       => ???
    }

    val vars: Map[String, (TYPE, LazyVal)] = Map(
      ("height", (LONG, LazyVal(EitherT(heightCoeval)))),
      ("tx", (outgoingTransactionType, LazyVal(EitherT(txCoeval))))
    )

    val functions = Seq(
      txByIdF,
      txHeightByIdF,
      getIntegerF,
      getBooleanF,
      getBinaryF,
      getStringF,
      addressFromBytesF,
      addressFromPublicKeyF,
      addressFromStringF,
      addressFromRecipientF,
      accountBalanceF,
      accountAssetBalanceF
    )

    CTX(Types.wavesTypes, vars, functions)
  }
}

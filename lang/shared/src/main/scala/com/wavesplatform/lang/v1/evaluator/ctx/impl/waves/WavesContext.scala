package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions
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
      NativeFunction(name, 100, internalName, OPTION(dataType.innerType), "addressOrAlias" -> addressType.typeRef, "key" -> STRING) {
        case (addressOrAlias: CaseObj) :: (k: String) :: Nil => environmentFunctions.getData(addressOrAlias, k, dataType)
        case _                                               => ???
      }

    val getLongF: BaseFunction      = getdataF("getLong", DATA_LONG, DataType.Long)
    val getBooleanF: BaseFunction   = getdataF("getBoolean", DATA_BOOLEAN, DataType.Boolean)
    val getByteArrayF: BaseFunction = getdataF("getByteArray", DATA_BYTES, DataType.ByteArray)
    val getStringF: BaseFunction    = getdataF("getString", DATA_STRING, DataType.String)

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

    val addressFromStringF: BaseFunction = NativeFunction("addressFromString", 100, ADDRESSFROMSTRING, optionAddress, "string" -> STRING) {
      case (addressString: String) :: Nil =>
        val r = environmentFunctions.addressFromString(addressString)
        r.map(_.map(x => CaseObj(addressType.typeRef, Map("bytes" -> x))))
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
      val returnType = OPTION(anyTransactionType)
      NativeFunction("getTransactionById", 100, GETTRANSACTIONBYID, returnType, "id" -> BYTEVECTOR) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
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

    val txHeightByIdF: BaseFunction = NativeFunction("transactionHeightById", 100, TRANSACTIONHEIGHTBYID, OPTION(LONG), "id" -> BYTEVECTOR) {
      case (id: ByteVector) :: Nil => Right(env.transactionHeightById(id.toArray))
      case _                       => ???
    }

    val vars: Map[String, (TYPE, LazyVal)] = Map(
      ("height", (LONG, LazyVal(EitherT(heightCoeval)))),
      ("tx", (outgoingTransactionType, LazyVal(EitherT(txCoeval))))
    )

    val functions = Seq(
      txByIdF,
      txHeightByIdF,
      getLongF,
      getBooleanF,
      getByteArrayF,
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

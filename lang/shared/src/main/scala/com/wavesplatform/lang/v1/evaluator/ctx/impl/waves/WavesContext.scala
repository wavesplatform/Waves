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

    def getdataF(name: String, internalName: Short, dataType: DataType): BaseFunction =
      NativeFunction(name, 100, internalName, UNION(dataType.innerType, UNIT), "addressOrAlias" -> addressOrAliasType, "key" -> STRING) {
        case (addressOrAlias: CaseObj) :: (k: String) :: Nil => environmentFunctions.getData(addressOrAlias, k, dataType).map(fromOption)
        case _                                               => ???
      }

    val getIntegerF: BaseFunction = getdataF("getInteger", DATA_LONG, DataType.Long)
    val getBooleanF: BaseFunction = getdataF("getBoolean", DATA_BOOLEAN, DataType.Boolean)
    val getBinaryF: BaseFunction  = getdataF("getBinary", DATA_BYTES, DataType.ByteArray)
    val getStringF: BaseFunction  = getdataF("getString", DATA_STRING, DataType.String)

    def secureHashExpr(xs: EXPR): EXPR = FUNCTION_CALL(
      FunctionHeader.Native(KECCAK256),
      List(
        FUNCTION_CALL(
          FunctionHeader.Native(BLAKE256),
          List(xs)
        )
      )
    )

    val addressFromPublicKeyF: BaseFunction = UserFunction("addressFromPublicKey", addressType.typeRef, "publicKey" -> BYTEVECTOR) {
      case pk :: Nil =>
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
                        secureHashExpr(pk),
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
      case _ => ???
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

    val addressFromStringF: BaseFunction = UserFunction("addressFromString", optionAddress, "string" -> STRING) {
      case (str: EXPR) :: Nil =>
        BLOCK(
          LET("@afs_addrBytes", FUNCTION_CALL(FunctionHeader.Native(FROMBASE58), List(removePrefixExpr(str, EnvironmentFunctions.AddressPrefix)))),
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
      NativeFunction("transactionById", 100, GETTRANSACTIONBYID, returnType, "id" -> BYTEVECTOR) {
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
      NativeFunction("assetBalance", 100, ACCOUNTASSETBALANCE, LONG, "addressOrAlias" -> addressOrAliasType, "assetId" -> UNION(UNIT, BYTEVECTOR)) {
        case (c: CaseObj) :: (()) :: Nil                  => env.accountBalanceOf(caseObjToRecipient(c), None)
        case (c: CaseObj) :: (assetId: ByteVector) :: Nil => env.accountBalanceOf(caseObjToRecipient(c), Some(assetId.toArray))

        case _ => ???
      }

    val wavesBalanceF: UserFunction = UserFunction("wavesBalance", LONG, "addressOrAlias" -> addressOrAliasType) {
      case aoa :: Nil => FUNCTION_CALL(assetBalanceF.header, List(aoa, REF("unit")))
      case _          => ???
    }

    val txHeightByIdF: BaseFunction = NativeFunction("transactionHeightById", 100, TRANSACTIONHEIGHTBYID, optionLong, "id" -> BYTEVECTOR) {
      case (id: ByteVector) :: Nil => Right(fromOption(env.transactionHeightById(id.toArray)))
      case _                       => ???
    }

    val vars: Map[String, (FINAL, LazyVal)] = Map(
      ("height", (com.wavesplatform.lang.v1.compiler.Types.LONG, LazyVal(EitherT(heightCoeval)))),
      ("input", (scriptInputType, LazyVal(EitherT(inputEntityCoeval))))
    )

    val functions = Seq(
      txByIdF,
      txHeightByIdF,
      getIntegerF,
      getBooleanF,
      getBinaryF,
      getStringF,
      addressFromPublicKeyF,
      addressFromStringF,
      addressFromRecipientF,
      assetBalanceF,
      wavesBalanceF
    )

    CTX(Types.wavesTypes, vars, functions)
  }
}

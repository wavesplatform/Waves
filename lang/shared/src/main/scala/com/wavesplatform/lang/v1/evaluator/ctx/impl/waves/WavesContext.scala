package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions
import com.wavesplatform.lang.v1.traits._
import monix.eval.Coeval
import scodec.bits.ByteVector

object WavesContext {

  import Bindings._
  import Types._

  def build(env: Environment): EvaluationContext = {
    val environmentFunctions = new EnvironmentFunctions(env)

    def getdataF(name: String, dataType: DataType) =
      PredefFunction(name, 100, OPTION(dataType.innerType), List(("address", addressType.typeRef), ("key", STRING))) {
        case (addr: CaseObj) :: (k: String) :: Nil => environmentFunctions.getData(addr, k, dataType)
        case _                                     => ???
      }

    val getLongF: PredefFunction      = getdataF("getLong", DataType.Long)
    val getBooleanF: PredefFunction   = getdataF("getBoolean", DataType.Boolean)
    val getByteArrayF: PredefFunction = getdataF("getByteArray", DataType.ByteArray)
    val getStringF: PredefFunction    = getdataF("getString", DataType.String)

    val addressFromPublicKeyF: PredefFunction = PredefFunction("addressFromPublicKey", 100, addressType.typeRef, List(("publicKey", BYTEVECTOR))) {
      case (pk: ByteVector) :: Nil =>
        val r = environmentFunctions.addressFromPublicKey(pk)
        Right(CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(r))))
      case _ => ???
    }

    val addressFromStringF: PredefFunction = PredefFunction("addressFromString", 100, optionAddress, List(("string", STRING))) {
      case (addressString: String) :: Nil =>
        val r = environmentFunctions.addressFromString(addressString)
        r.map(_.map(x => CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(x)))))
      case _ => ???
    }

    val addressFromRecipientF: PredefFunction =
      PredefFunction("addressFromRecipient", 100, addressType.typeRef, List(("AddressOrAlias", addressOrAliasType))) {
        case (c @ CaseObj(addressType.typeRef, _)) :: Nil => Right(c)
        case c @ CaseObj(aliasType.typeRef, fields) :: Nil =>
          environmentFunctions
            .addressFromAlias(fields("name").value.asInstanceOf[String])
            .map(resolved => CaseObj(addressType.typeRef, Map("bytes" -> Val(BYTEVECTOR)(resolved.bytes))))
        case _ => ???
      }

    val txCoeval: Coeval[Either[String, CaseObj]]  = Coeval.evalOnce(Right(transactionObject(env.transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(env.height))

    val txByIdF = {
      val returnType = OPTION(anyTransactionType)
      PredefFunction("getTransactionById", 100, returnType, List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil =>
          val maybeDomainTx = env.transactionById(id.toArray).map(transactionObject)
          Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
        case _ => ???
      }
    }

    val accountBalanceF: PredefFunction =
      PredefFunction("accountBalance", 100, LONG, List(("addressOrAlias", addressOrAliasType))) {
        case CaseObj(_, fields) :: Nil =>
          val acc = fields("bytes").value.asInstanceOf[ByteVector].toArray
          env.accountBalanceOf(acc, None)

        case _ => ???
      }

    val accountAssetBalanceF: PredefFunction =
      PredefFunction("accountAssetBalance", 100, LONG, List(("addressOrAlias", addressOrAliasType), ("assetId", BYTEVECTOR))) {
        case CaseObj(_, fields) :: (assetId: ByteVector) :: Nil =>
          val acc = fields("bytes").value.asInstanceOf[ByteVector]
          env.accountBalanceOf(acc.toArray, Some(assetId.toArray))

        case _ => ???
      }

    val txHeightByIdF =
      PredefFunction("transactionHeightById", 100, OPTION(LONG), List(("id", BYTEVECTOR))) {
        case (id: ByteVector) :: Nil => Right(env.transactionHeightById(id.toArray))
        case _                       => ???
      }

    EvaluationContext.build(
      caseTypes = Seq(addressType, aliasType, transfer) ++ activeTransactionTypes,
      letDefs = Map(("height", LazyVal(LONG)(EitherT(heightCoeval))), ("tx", LazyVal(outgoingTransactionType)(EitherT(txCoeval)))),
      functions = Seq(
        txByIdF,
        txHeightByIdF,
        getLongF,
        getBooleanF,
        getByteArrayF,
        getStringF,
        addressFromPublicKeyF,
        addressFromStringF,
        addressFromRecipientF,
        accountBalanceF,
        accountAssetBalanceF
      )
    )
  }
}

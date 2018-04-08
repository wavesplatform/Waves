package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx._
import com.wavesplatform.lang.traits._
import monix.eval.Coeval
import scodec.bits.ByteVector

abstract class BaseWavesContext extends Environment {

  import BaseWavesContext._

  private val Global = com.wavesplatform.lang.hacks.Global // Hack for IDEA




  private val txByIdF = {
    val returnType = OPTION(transactionType.typeRef)
    PredefFunction("getTransactionById", returnType, List(("id", BYTEVECTOR))) {
      case (id: ByteVector) :: Nil =>
        val maybeDomainTx = transactionById(id.toArray).map(transactionObject)
        Right(maybeDomainTx).map(_.asInstanceOf[returnType.Underlying])
      case _ => ???
    }
  }

  def build(): Context = {
    val txCoeval: Coeval[Either[String, Obj]]      = Coeval.evalOnce(Right(transactionObject(transaction)))
    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Right(height))
    Context(
      Map(transactionType.name -> transactionType, addressType.name -> addressType, addressOrAliasType.name -> addressOrAliasType),
      Map(
        ("None", none),
        ("height", LazyVal(LONG)(EitherT(heightCoeval))),
        ("tx", LazyVal(TYPEREF(transactionType.name))(EitherT(txCoeval)))
      ),
      Map(
        sigVerifyF.header -> sigVerifyF,
        extract.header    -> extract,
        isDefined.header  -> isDefined,
        some.header       -> some,
        size.header       -> size,
        //hashing
        keccak256F.header  -> keccak256F,
        blake2b256F.header -> blake2b256F,
        sha256F.header     -> sha256F,
        //utils
        toBase58StringF.header -> toBase58StringF,
        //dsl
        addressFromPublicKeyF.header -> addressFromPublicKeyF,
        addressFromStringF.header    -> addressFromStringF,
        //state
        txByIdF.header               -> txByIdF,
        getLongF.header              -> getLongF,
        getBooleanF.header           -> getBooleanF,
        getByteArrayF.header         -> getByteArrayF,
        addressFromRecipientF.header -> addressFromRecipientF
      )
    )
  }

}

object BaseWavesContext {

}

package com.wavesplatform.lang.contract

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import cats.implicits._
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.utils.Serialize._
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.Serde.desAux
import com.wavesplatform.lang.v1.compiler.Terms.{DECLARATION, FUNC}
import monix.eval.Coeval

import scala.util.Try

object ContractSerDe {

  val CALL_ANNO: Int = 1
  val VER_ANNO: Int  = 3

  def serialize(c: DApp): Array[Byte] = {
    val out = new ByteArrayOutputStream()

    out.writeInt(0)
    out.writeInt(c.dec.size)
    c.dec.foreach(dec => serializeDeclaration(out, dec))
    out.writeInt(c.cfs.size)
    c.cfs.foreach(cFunc => serializeAnnotatedFunction(out, cFunc.u, cFunc.annotation.invocationArgName))

    c.vf match {
      case None =>
        out.writeInt(0)
      case Some(vf) =>
        out.writeInt(1)
        serializeAnnotatedFunction(out, vf.u, vf.annotation.invocationArgName)
    }

    out.toByteArray
  }

  def deserialize(arr: Array[Byte]): Either[String, DApp] = {
    val bb = ByteBuffer.wrap(arr)
    for {
      _    <- tryEi(bb.getInt())
      decs <- deserializeList[DECLARATION](bb, deserializeDeclaration)
      cfs  <- deserializeList(bb, deserializeCallableFunction)
      vf   <- deserializeOption(bb, deserializeVerifierFunction)
    } yield DApp(decs, cfs, vf)
  }

  private def serializeDeclaration(out: ByteArrayOutputStream, dec: DECLARATION): Unit = {
    Serde.serializeDeclaration(out, dec, Serde.serAux(out, Coeval.now(()), _)).value
  }

  private def deserializeDeclaration(bb: ByteBuffer): Either[String, DECLARATION] = {
    val decType = bb.get()
    Serde.deserializeDeclaration(bb, desAux(bb), decType).attempt.value.leftMap(_.getMessage)
  }

  private[lang] def serializeAnnotation(out: ByteArrayOutputStream, invocationName: String): Unit = {
    out.writeString(invocationName)
  }

  private[lang] def serializeAnnotatedFunction(out: ByteArrayOutputStream, func: FUNC, annotationInvocName: String): Unit = {
    serializeAnnotation(out, annotationInvocName)
    serializeDeclaration(out, func)
  }

  private[lang] def deserializeCallableAnnotation(bb: ByteBuffer): Either[String, CallableAnnotation] =
    tryEi(CallableAnnotation(bb.getString))

  private[lang] def deserializeCallableFunction(bb: ByteBuffer): Either[String, CallableFunction] = {
    for {
      ca <- deserializeCallableAnnotation(bb)
      cf <- deserializeDeclaration(bb).map(_.asInstanceOf[FUNC])
      _  <- Either.cond(cf.name.getBytes().size <= ContractLimits.MaxCallableFunctionNameInBytes, (), s"Callable function name (${cf.name}) longer than limit ${ContractLimits.MaxCallableFunctionNameInBytes}")
    } yield CallableFunction(ca, cf)
  }

  private[lang] def deserializeVerifiableAnnotation(bb: ByteBuffer): Either[String, VerifierAnnotation] =
    tryEi(VerifierAnnotation(bb.getString))

  private def deserializeVerifierFunction(bb: ByteBuffer): Either[String, VerifierFunction] = {
    for {
      a <- deserializeVerifiableAnnotation(bb)
      f <- deserializeDeclaration(bb).map(_.asInstanceOf[FUNC])
    } yield VerifierFunction(a, f)
  }

  private[lang] def deserializeList[A](bb: ByteBuffer, df: ByteBuffer => Either[String, A]): Either[String, List[A]] =
    (1 to bb.getInt).toList
      .traverse[({ type L[B] = Either[String, B] })#L, A](_ => df(bb))

  private[lang] def deserializeOption[A](bb: ByteBuffer, df: ByteBuffer => Either[String, A]): Either[String, Option[A]] = {
    tryEi(bb.getInt > 0)
      .flatMap {
        case true  => df(bb).map(_.some)
        case false => Right(None)
      }
  }

  private[lang] def deserializeFromArray[A](bb: ByteBuffer, df: Array[Byte] => Either[String, A]): Either[String, A] = {
    tryEi {
      val len = bb.getInt()
      val arr = new Array[Byte](len)
      bb.get(arr, bb.arrayOffset(), len)
      arr
    } flatMap df
  }

  private[lang] def tryEi[A](f: => A): Either[String, A] = Try(f).toEither.leftMap(_.getMessage)
}

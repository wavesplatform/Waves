package com.wavesplatform.lang.contract

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import cats.implicits._
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.utils.Serialize._
import com.wavesplatform.lang.v1.Serde.desAux
import com.wavesplatform.lang.v1.compiler.Terms.{DECLARATION, FUNC}
import com.wavesplatform.lang.v1.{ContractLimits, Serde}
import com.wavesplatform.protobuf.dapp.DAppMeta
import monix.eval.Coeval

import scala.util.Try

object ContractSerDe {

  val CALL_ANNO: Int = 1
  val VER_ANNO: Int  = 3

  def serialize(c: DApp): Either[String, Array[Byte]] =
    for {
      metaBytes <- {
        val metaBytes = c.meta.toByteArray
        checkMetaSize(metaBytes.length).map(_ => metaBytes)
      }
      out <- tryEi {
        val out = new ByteArrayOutputStream()

        // version byte
        out.writeInt(0)

        out.writeInt(metaBytes.length)
        out.write(metaBytes)

        out.writeInt(c.decs.size)
        c.decs.foreach(dec => serializeDeclaration(out, dec))

        out.writeInt(c.callableFuncs.size)
        c.callableFuncs.foreach(cFunc => serializeAnnotatedFunction(out, cFunc.u, cFunc.annotation.invocationArgName))

        c.verifierFuncOpt match {
          case None     => out.writeInt(0)
          case Some(vf) =>
            out.writeInt(1)
            serializeAnnotatedFunction(out, vf.u, vf.annotation.invocationArgName)
        }
        out
      }
    } yield out.toByteArray

  def deserialize(arr: Array[Byte]): Either[String, DApp] = {
    val bb = ByteBuffer.wrap(arr)
    for {
      _               <- tryEi(bb.getInt())
      meta            <- deserializeMeta(bb)
      decs            <- deserializeList[DECLARATION](bb, deserializeDeclaration)
      callableFuncs   <- deserializeList(bb, deserializeCallableFunction)
      verifierFuncOpt <- deserializeOption(bb, deserializeVerifierFunction)
    } yield DApp(meta, decs, callableFuncs, verifierFuncOpt)
  }

  private def checkMetaSize(metaSize: Int): Either[String, Unit] = {
    Either.cond(
      metaSize <= ContractLimits.MaxContractMetaSizeInBytes,
      (),
      s"Script meta size in bytes must be not greater than ${ContractLimits.MaxContractMetaSizeInBytes}"
    )
  }

  private[lang] def deserializeMeta(bb: ByteBuffer): Either[String, DAppMeta] =
    for {
      size <- tryEi(bb.getInt)
      _    <- checkMetaSize(size)
      meta <- tryEi {
        val arr = new Array[Byte](size)
        bb.get(arr, bb.arrayOffset(), size)
        DAppMeta.parseFrom(arr)
      }
    } yield meta

  private[lang]  def serializeDeclaration(out: ByteArrayOutputStream, dec: DECLARATION): Unit = {
    Serde.serializeDeclaration(out, dec, Serde.serAux(out, Coeval.now(()), _)).value
  }

  private[lang]  def deserializeDeclaration(bb: ByteBuffer): Either[String, DECLARATION] = {
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
      _ <- Either.cond(
        cf.name.getBytes("UTF-8").size <= ContractLimits.MaxAnnotatedFunctionNameInBytes,
        (),
        s"Callable function name (${cf.name}) longer than limit ${ContractLimits.MaxAnnotatedFunctionNameInBytes}"
      )
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

  private[lang] def deserializeList[A](bb: ByteBuffer, df: ByteBuffer => Either[String, A]): Either[String, List[A]] = {
    val len = bb.getInt
    if (len <= (bb.limit() - bb.position()) && len >= 0) {
      (1 to len).toList
        .traverse[Either[String, ?], A](_ => df(bb))
    } else {
      Left(s"At position ${bb.position()} array of arguments too big.")
    }
  }

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
      if (len <= (bb.limit() - bb.position()) && len >= 0) {
        val arr = new Array[Byte](len)
        bb.get(arr, bb.arrayOffset(), len)
        arr
      } else {
        throw new Exception(s"At position ${bb.position()} array of arguments too big.")
      }
    } flatMap df
  }

  private[lang] def tryEi[A](f: => A): Either[String, A] = Try(f).toEither.leftMap { e =>
    if (e.getMessage != null) e.getMessage else e.toString
  }
}

package com.wavesplatform.lang.contract.serialization

import cats.instances.list.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.v1.compiler.Terms.{DECLARATION, FUNC}
import com.wavesplatform.lang.utils.Serialize.*
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.protobuf.dapp.DAppMeta
import monix.eval.Coeval

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

object ContractSerDeV1 extends ContractSerDe {

  def serialize(c: DApp): Either[String, Array[Byte]] =
    for {
      out <- tryEi {
        val out = new ByteArrayOutputStream()

        val metaBytes = c.meta.toByteArray

        // version byte
        out.writeInt(0)

        out.writeInt(metaBytes.length)
        out.write(metaBytes)

        out.writeInt(c.decs.size)
        c.decs.foreach(dec => serializeDeclaration(out, dec))

        out.writeInt(c.callableFuncs.size)
        c.callableFuncs.foreach(cFunc => serializeAnnotatedFunction(out, cFunc.u, cFunc.annotation.invocationArgName))

        c.verifierFuncOpt match {
          case None => out.writeInt(0)
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

  private[lang] def deserializeMeta(bb: ByteBuffer): Either[String, DAppMeta] =
    for {
      size <- tryEi(bb.getInt)
      meta <- tryEi {
        val arr = new Array[Byte](size)
        bb.get(arr, bb.arrayOffset(), size)
        DAppMeta.parseFrom(arr)
      }
    } yield meta

  private[lang] def serializeDeclaration(out: ByteArrayOutputStream, dec: DECLARATION): Unit = {
    SerdeV1.serializeDeclaration(out, dec, SerdeV1.serAux(out, Coeval.now(()), _)).value()
  }

  private[lang] def deserializeDeclaration(bb: ByteBuffer): Either[String, DECLARATION] = {
    val decType = bb.get()
    SerdeV1.deserializeDeclaration(bb, SerdeV1.desAux(bb), decType).attempt.value().leftMap(_.getMessage)
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
      nameSize = cf.name.getBytes(StandardCharsets.UTF_8).length
      _ <- Either.cond(
        nameSize <= ContractLimits.MaxDeclarationNameInBytes,
        (),
        s"Callable function name (${cf.name}) size = $nameSize bytes exceeds ${ContractLimits.MaxDeclarationNameInBytes}"
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
        .traverse[Either[String, *], A](_ => df(bb))
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
}

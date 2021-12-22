package com.wavesplatform.lang.contract.serialization

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.{DECLARATION, FUNC}
import com.wavesplatform.lang.v1.serialization.SerdeV2
import com.wavesplatform.protobuf.dapp.DAppMeta
import monix.eval.Coeval

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

object ContractSerDeV2 extends ContractSerDe {

  def serialize(c: DApp): Either[String, Array[Byte]] =
    for {
      out <- tryEi {
        val internalOut = new ByteArrayOutputStream()
        val out         = CodedOutputStream.newInstance(internalOut)

        val metaBytes = c.meta.toByteArray

        // version byte
        out.writeRawByte(0)

        out.writeByteArrayNoTag(metaBytes)

        out.writeUInt32NoTag(c.decs.size)
        c.decs.foreach(dec => serializeDeclaration(out, dec))

        out.writeUInt32NoTag(c.callableFuncs.size)
        c.callableFuncs.foreach(cFunc => serializeAnnotatedFunction(out, cFunc.u, cFunc.annotation.invocationArgName))

        c.verifierFuncOpt match {
          case None => out.writeRawByte(0)
          case Some(vf) =>
            out.writeRawByte(1)
            serializeAnnotatedFunction(out, vf.u, vf.annotation.invocationArgName)
        }
        out.flush()
        internalOut
      }
    } yield out.toByteArray

  def deserialize(arr: Array[Byte]): Either[String, DApp] = {
    val in = CodedInputStream.newInstance(arr)
    for {
      _               <- tryEi(in.readRawByte())
      meta            <- deserializeMeta(in)
      decs            <- deserializeList[DECLARATION](in, deserializeDeclaration)
      callableFuncs   <- deserializeList(in, deserializeCallableFunction)
      verifierFuncOpt <- deserializeOption(in, deserializeVerifierFunction)
    } yield DApp(meta, decs, callableFuncs, verifierFuncOpt)
  }

  private[lang] def deserializeMeta(in: CodedInputStream): Either[String, DAppMeta] =
    tryEi(DAppMeta.parseFrom(in.readByteArray()))

  private[lang] def serializeDeclaration(out: CodedOutputStream, dec: DECLARATION): Unit = {
    SerdeV2.serializeDeclaration(out, dec, SerdeV2.serAux(out, Coeval.now(()), _)).value()
  }

  private[lang] def deserializeDeclaration(in: CodedInputStream): Either[String, DECLARATION] = {
    val decType = in.readRawByte()
    SerdeV2.deserializeDeclaration(in, SerdeV2.desAux(in), decType).attempt.value().leftMap(_.getMessage)
  }

  private[lang] def serializeAnnotation(out: CodedOutputStream, invocationName: String): Unit = {
    out.writeStringNoTag(invocationName)
  }

  private[lang] def serializeAnnotatedFunction(out: CodedOutputStream, func: FUNC, annotationInvocName: String): Unit = {
    serializeAnnotation(out, annotationInvocName)
    serializeDeclaration(out, func)
  }

  private[lang] def deserializeCallableAnnotation(in: CodedInputStream): Either[String, CallableAnnotation] =
    tryEi(CallableAnnotation(in.readString()))

  private[lang] def deserializeCallableFunction(in: CodedInputStream): Either[String, CallableFunction] = {
    for {
      ca <- deserializeCallableAnnotation(in)
      cf <- deserializeDeclaration(in).map(_.asInstanceOf[FUNC])
      nameSize = cf.name.getBytes(StandardCharsets.UTF_8).length
      _ <- Either.cond(
        nameSize <= ContractLimits.MaxDeclarationNameInBytes,
        (),
        s"Callable function name (${cf.name}) size = $nameSize bytes exceeds ${ContractLimits.MaxDeclarationNameInBytes}"
      )
    } yield CallableFunction(ca, cf)
  }

  private[lang] def deserializeVerifiableAnnotation(in: CodedInputStream): Either[String, VerifierAnnotation] =
    tryEi(VerifierAnnotation(in.readString()))

  private def deserializeVerifierFunction(in: CodedInputStream): Either[String, VerifierFunction] = {
    for {
      a <- deserializeVerifiableAnnotation(in)
      f <- deserializeDeclaration(in).map(_.asInstanceOf[FUNC])
    } yield VerifierFunction(a, f)
  }

  private[lang] def deserializeList[A](in: CodedInputStream, df: CodedInputStream => Either[String, A]): Either[String, List[A]] = {
    val len = in.readUInt32()
    (1 to len).toList.traverse[Either[String, *], A](_ => df(in))
  }

  private[lang] def deserializeOption[A](in: CodedInputStream, df: CodedInputStream => Either[String, A]): Either[String, Option[A]] = {
    tryEi(in.readRawByte() > 0)
      .flatMap {
        case true  => df(in).map(_.some)
        case false => Right(None)
      }
  }
}

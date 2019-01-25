package com.wavesplatform.lang.contract

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import cats.implicits._
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.utils.Serialize._
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.Terms.{DECLARATION, FUNC, LET}

import scala.util.Try

object ContractSerDe {

  val CALL_ANNO: Int = 1
  val VER_ANNO: Int  = 3

  val DEC_LET: Int  = 1
  val DEC_FUNC: Int = 2

  def serialize(c: Contract): Array[Byte] = {
    val out = new ByteArrayOutputStream()

    out.writeInt(0)
    out.writeInt(c.dec.size)
    c.dec.foreach(serializeDeclaration(out, _))
    out.writeInt(c.cfs.size)
    c.cfs.foreach(serializeContractFunction(out, _))

    c.vf match {
      case None =>
        out.writeInt(0)
      case Some(vf) =>
        out.writeInt(1)
        serializeVerifierFunction(out, vf)
    }

    out.toByteArray
  }

  def deserialize(arr: Array[Byte]): Either[String, Contract] = {
    val bb = ByteBuffer.wrap(arr)

    for {
      _    <- tryEi(bb.getInt())
      decs <- deserializeList(bb, deserializeDeclaration)
      cfs  <- deserializeList(bb, deserializeContractFunction)
      vf   <- deserializeOption(bb, deserializeVerifierFunction)
    } yield Contract(decs, cfs, vf)
  }

  private[lang] def serializeAnnotation(out: ByteArrayOutputStream, a: Annotation): Unit = {
    a match {
      case c: CallableAnnotation =>
        out.writeInt(CALL_ANNO)
        serializeCallAnnotation(out, c)
      case v: VerifierAnnotation =>
        out.writeInt(VER_ANNO)
        serializeVerifiableAnnotation(out, v)
    }
  }

  private[lang] def deserializeAnnotation(bb: ByteBuffer): Either[String, Annotation] = {
    for {
      annType <- tryEi(bb.getInt)
      ann <- annType match {
        case CALL_ANNO => deserializeCallAnnotation(bb)
        case VER_ANNO  => deserializeVerifiableAnnotation(bb)
        case t         => Left(s"Unknown annotation type: $t")
      }
    } yield ann
  }

  private[lang] def deserializeCallAnnotation(bb: ByteBuffer): Either[String, CallableAnnotation] =
    tryEi(CallableAnnotation(bb.getString))

  private[lang] def deserializeVerifiableAnnotation(bb: ByteBuffer): Either[String, VerifierAnnotation] =
    tryEi(VerifierAnnotation(bb.getString))

  private[lang] def serializeCallAnnotation(out: ByteArrayOutputStream, a: CallableAnnotation): Unit =
    out.writeString(a.invocationArgName)

  private[lang] def serializeVerifiableAnnotation(out: ByteArrayOutputStream, v: VerifierAnnotation): Unit = {
    out.writeString(v.txArgName)
  }

  private[lang] def serializeDeclaration(out: ByteArrayOutputStream, d: DECLARATION): Unit = {
    d match {
      case l: LET =>
        out.writeInt(DEC_LET)
        serializeLET(out, l)
      case f: FUNC =>
        out.writeInt(DEC_FUNC)
        serializeFUNC(out, f)
    }
  }

  private[lang] def deserializeDeclaration(bb: ByteBuffer): Either[String, DECLARATION] = {
    for {
      decType <- tryEi(bb.getInt)
      dec <- decType match {
        case DEC_LET  => deserializeLET(bb)
        case DEC_FUNC => deserializeFUNC(bb)
      }
    } yield dec
  }

  private[lang] def serializeLET(out: ByteArrayOutputStream, l: LET): Unit = {
    out.writeString(l.name)
    val expr = Serde.serialize(l.value)
    out.writeInt(expr.length)
    out.write(expr)
  }

  private[lang] def deserializeLET(bb: ByteBuffer): Either[String, LET] = {
    for {
      name <- tryEi(bb.getString)
      body <- deserializeFromArray(bb, Serde.deserialize(_, all = true))
    } yield LET(name, body._1)
  }

  private[lang] def serializeFUNC(out: ByteArrayOutputStream, f: FUNC): Unit = {

    out.writeString(f.name)
    out.writeInt(f.args.size)
    f.args.foreach(out.writeString)
    val expr = Serde.serialize(f.body)
    out.writeInt(expr.length)
    out.write(expr)
  }

  private[lang] def deserializeFUNC(bb: ByteBuffer): Either[String, FUNC] = {
    type G[A] = Either[String, A]
    for {
      name <- tryEi(bb.getString)
      argc <- tryEi(bb.getInt)
      args <- (1 to argc).toList
        .traverse[G, String](_ => tryEi(bb.getString))
      expr <- deserializeFromArray(bb, Serde.deserialize(_, all = true))
    } yield FUNC(name, args, expr._1)
  }

  private[lang] def serializeContractFunction(out: ByteArrayOutputStream, cf: ContractFunction): Unit = {
    serializeCallAnnotation(out, cf.annotation)
    serializeFUNC(out, cf.u)
  }

  private[lang] def deserializeContractFunction(bb: ByteBuffer): Either[String, ContractFunction] = {
    for {
      ca        <- deserializeCallAnnotation(bb)
      cf <- deserializeFUNC(bb)
    } yield ContractFunction(ca, cf)
  }

  def serializeVerifierFunction(out: ByteArrayOutputStream, vf: VerifierFunction): Unit = {
    serializeVerifiableAnnotation(out, vf.annotation)
    serializeFUNC(out, vf.u)
  }

  def deserializeVerifierFunction(bb: ByteBuffer): Either[String, VerifierFunction] = {
    for {
      a <- deserializeVerifiableAnnotation(bb)
      f <- deserializeFUNC(bb)
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

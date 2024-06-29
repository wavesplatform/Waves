package com.wavesplatform.lang.v1.serialization

import cats.instances.lazyList.*
import cats.instances.list.*
import cats.syntax.apply.*
import cats.syntax.traverse.*
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.serialization.Serde.*
import com.wavesplatform.lang.utils.Serialize.*
import monix.eval.Coeval

import java.io.ByteArrayOutputStream
import scala.util.Try

object SerdeV2 extends Serde[CodedInputStream, CodedOutputStream] {

  def serializeDeclaration(out: CodedOutputStream, dec: DECLARATION, aux: EXPR => Coeval[Unit]): Coeval[Unit] = {
    dec match {
      case LET(name, value) =>
        Coeval.now {
          out.writeRawByte(DEC_LET)
          out.writeStringNoTag(name)
        } *> aux(value)
      case FUNC(name, args, body) =>
        Coeval.now {
          out.writeRawByte(DEC_FUNC)
          out.writeStringNoTag(name)
          out.writeRawByte(args.size.toByte)
          args.foreach(out.writeStringNoTag)
        } *> aux(body)
      case _: FAILED_DEC =>
        Coeval.raiseError(new Exception("Attempt to serialize failed declaration."))
    }
  }

  def deserializeDeclaration(in: CodedInputStream, aux: => Coeval[EXPR], decType: Byte): Coeval[DECLARATION] = {
    (decType: @unchecked) match {
      case DEC_LET =>
        for {
          name <- Coeval.now(in.readString())
          body <- aux
        } yield LET(name, body)
      case DEC_FUNC =>
        deserializeFunction(in, aux)
    }
  }

  def deserializeFunction(in: CodedInputStream, aux: => Coeval[EXPR]): Coeval[FUNC] =
    for {
      name <- Coeval.now(in.readString())
      args <- {
        val argsCnt = in.readRawByte()
        Coeval {
          (1 to argsCnt).toList.map(_ => in.readString())
        }
      }
      body <- aux
    } yield FUNC(name, args, body)

  def desAux(in: CodedInputStream, allowObjects: Boolean = false, acc: Coeval[Unit] = Coeval.now(())): Coeval[EXPR] =
    desAuxR(in, allowObjects, acc)

  private def desAuxR(in: CodedInputStream, allowObjects: Boolean, acc: Coeval[Unit]): Coeval[EXPR] = acc.flatMap { _ =>
    (in.readRawByte(): @unchecked) match {
      case E_LONG   => Coeval.now(CONST_LONG(in.readInt64()))
      case E_BYTES  => Coeval.now(CONST_BYTESTR(ByteStr(in.readByteArray())).explicitGet())
      case E_STRING => Coeval.now(CONST_STRING(in.readString()).explicitGet())
      case E_IF     => (desAuxR(in, allowObjects, acc), desAuxR(in, allowObjects, acc), desAuxR(in, allowObjects, acc)).mapN(IF.apply)
      case E_BLOCK =>
        for {
          name     <- Coeval.now(in.readString())
          letValue <- desAuxR(in, allowObjects, acc)
          body     <- desAuxR(in, allowObjects, acc)
        } yield LET_BLOCK(
          let = LET(name, letValue),
          body = body
        )
      case E_BLOCK_V2 =>
        for {
          decType <- Coeval.now(in.readRawByte())
          dec     <- deserializeDeclaration(in, desAuxR(in, allowObjects, acc), decType)
          body    <- desAuxR(in, allowObjects, acc)
        } yield BLOCK(dec, body)
      case E_REF    => Coeval.now(REF(in.readString()))
      case E_TRUE   => Coeval.now(TRUE)
      case E_FALSE  => Coeval.now(FALSE)
      case E_GETTER => desAuxR(in, allowObjects, acc).map(GETTER(_, field = in.readString()))
      case E_FUNCALL =>
        Coeval
          .now((in.getFunctionHeader, in.readRawByte()))
          .flatMap {
            case (header, argc) =>
              (1 to argc)
                .map(_ => desAuxR(in, allowObjects, acc))
                .toList
                .sequence
                .map(FUNCTION_CALL(header, _))
          }
      case E_ARR =>
        Coeval
          .now(in.readUInt32())
          .flatMap { argsCount =>
            (1 to argsCount)
              .to(LazyList)
              .traverse(_ => evaluatedOnly(desAuxR(in, allowObjects, acc)))
              .map(elements => ARR(elements.toIndexedSeq, limited = false).explicitGet())
          }
      case E_CASE_OBJ if allowObjects =>
        for {
          (typeName, fieldsNumber) <- Coeval((in.readString(), in.readRawByte()))
          fields <- (1 to fieldsNumber)
            .to(LazyList)
            .traverse { _ =>
              for {
                fieldName <- Coeval.now(in.readString())
                fieldValue <- evaluatedOnly(desAuxR(in, allowObjects, acc))
              } yield (fieldName, fieldValue)
            }
        } yield CaseObj(CASETYPEREF(typeName, Nil), fields.toMap)
    }
  }

  def deserialize(bytes: Array[Byte], all: Boolean = true, allowObjects: Boolean = false): Either[String, (EXPR, Int)] = {
    val in = CodedInputStream.newInstance(bytes)
    val res = Try(desAux(in, allowObjects).value()).toEither.left
      .map(_.getMessage)
    (if (all)
      res.flatMap { r =>
        if (!in.isAtEnd) Left(s"${in.getBytesUntilLimit} bytes left")
        else Right(r)
      } else res)
      .map((_, in.getBytesUntilLimit))
  }

  def deserialize(in: CodedInputStream): Either[String, EXPR] =
    Try(desAux(in).value()).toEither.left.map(_.getMessage)

  def deserializeFunctionCall(in: CodedInputStream): Either[Throwable, FUNCTION_CALL] =
    Try(desAux(in).value()).toEither.flatMap {
      case fc: FUNCTION_CALL => Right(fc)
      case other => Left(new RuntimeException(s"Not a function call: $other"))
    }

  def deserializeFunctionCall(array: Array[Byte]): Either[Throwable, FUNCTION_CALL] =
    deserializeFunctionCall(CodedInputStream.newInstance(array))

  def serAux(out: CodedOutputStream, acc: Coeval[Unit], expr: EXPR, allowObjects: Boolean = false): Coeval[Unit] = acc.flatMap { _ =>
    expr match {
      case CONST_LONG(n) =>
        Coeval.now {
          out.writeRawByte(E_LONG)
          out.writeInt64NoTag(n)
        }
      case CONST_BYTESTR(bs) =>
        Coeval.now {
          out.writeRawByte(E_BYTES)
          out.writeByteArrayNoTag(bs.arr)
        }
      case CONST_STRING(s) =>
        Coeval.now {
          out.writeRawByte(E_STRING)
          out.writeStringNoTag(s)
        }
      case IF(cond, ifTrue, ifFalse) =>
        List(cond, ifTrue, ifFalse).foldLeft(Coeval.now(out.writeRawByte(E_IF)))((acc, expr) => serAux(out, acc, expr, allowObjects))
      case LET_BLOCK(LET(name, value), body) =>
        val n = Coeval.now[Unit] {
          out.writeRawByte(E_BLOCK)
          out.writeStringNoTag(name)
        }
        List(value, body).foldLeft(n)((acc, expr) => serAux(out, acc, expr, allowObjects))
      case BLOCK(dec, body) =>
        val n = Coeval.now[Unit] {
          out.writeRawByte(E_BLOCK_V2)
        }
        serAux(out, serializeDeclaration(out, dec, serAux(out, n, _, allowObjects)), body, allowObjects)
      case REF(key) =>
        Coeval.now {
          out.writeRawByte(E_REF)
          out.writeStringNoTag(key)
        }
      case CONST_BOOLEAN(b) =>
        Coeval.now(
          out.writeRawByte(
            if (b)
              E_TRUE
            else
              E_FALSE
          )
        )
      case GETTER(obj, field) =>
        serAux(out, Coeval.now[Unit](out.writeRawByte(E_GETTER)), obj, allowObjects).map { _ =>
          out.writeStringNoTag(field)
        }
      case FUNCTION_CALL(header, args) =>
        val n = Coeval.now[Unit] {
          out.writeRawByte(E_FUNCALL)
          out.writeFunctionHeader(header)
          out.writeRawByte(args.size.toByte)
        }
        args.foldLeft(n)((acc, arg) => serAux(out, acc, arg, allowObjects))

      case ARR(elements) =>
        val dataInfo = Coeval.now[Unit] {
          out.writeRawByte(E_ARR)
          out.writeUInt32NoTag(elements.size)
        }
        elements.foldLeft(dataInfo)((acc, element) => serAux(out, acc, element, allowObjects))

      case CaseObj(caseType, fields) if allowObjects =>
        val dataInfo = Coeval.now[Unit] {
          out.writeRawByte(E_CASE_OBJ)
          out.writeStringNoTag(caseType.name)
          out.writeRawByte(fields.size.toByte)
        }
        fields.foldLeft(dataInfo) {
          case (acc, (fieldName, fieldValue)) =>
            for {
              _ <- Coeval.now(out.writeStringNoTag(fieldName))
              r <- serAux(out, acc, fieldValue, allowObjects)
            } yield r
        }

      case x =>
        Coeval.raiseError(new Exception(s"Serialization of value $x is unsupported"))
    }
  }

  def serialize(expr: EXPR, allowObjects: Boolean = false): Array[Byte] = {
    val internalOut = new ByteArrayOutputStream()
    val out = CodedOutputStream.newInstance(internalOut)
    serAux(out, acc = Coeval.now(()), expr, allowObjects).value()
    out.flush()
    internalOut.toByteArray
  }
}

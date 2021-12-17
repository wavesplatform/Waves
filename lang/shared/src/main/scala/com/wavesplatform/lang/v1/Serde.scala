package com.wavesplatform.lang.v1

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import scala.util.Try
import cats.instances.lazyList._
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.traverse._
import com.google.protobuf.CodedOutputStream
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.utils.Serialize._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import monix.eval.Coeval

object Serde {
  val E_LONG: Byte    = 0
  val E_BYTES: Byte   = 1
  val E_STRING: Byte  = 2
  val E_IF: Byte      = 3
  val E_BLOCK: Byte   = 4
  val E_REF: Byte     = 5
  val E_TRUE: Byte    = 6
  val E_FALSE: Byte   = 7
  val E_GETTER: Byte  = 8
  val E_FUNCALL: Byte = 9

  val FH_NATIVE: Byte = 0
  val FH_USER: Byte   = 1

  val E_BLOCK_V2: Byte = 10
  val E_ARR: Byte      = 11
  val E_CASE_OBJ: Byte = 12

  val DEC_LET: Byte  = 0
  val DEC_FUNC: Byte = 1

  def serializeDeclaration(out: ByteArrayOutputStream, dec: DECLARATION, aux: EXPR => Coeval[Unit]): Coeval[Unit] = {
    dec match {
      case LET(name, value) =>
        Coeval.now {
          out.write(DEC_LET)
          out.writeString(name)
        } *> aux(value)
      case FUNC(name, args, body) =>
        Coeval.now {
          out.write(DEC_FUNC)
          out.writeString(name)
          out.writeInt(args.size)
          args.foreach(out.writeString)
        } *> aux(body)
      case _: FAILED_DEC =>
        Coeval.raiseError(new Exception("Attempt to serialize failed declaration."))
    }
  }

  def serializeDeclarationOptimized(out: CodedOutputStream, dec: DECLARATION, aux: EXPR => Coeval[Unit]): Coeval[Unit] = {
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

  def deserializeDeclaration(bb: ByteBuffer, aux: => Coeval[EXPR], decType: Byte): Coeval[DECLARATION] = {
    (decType: @unchecked) match {
      case DEC_LET =>
        for {
          name <- Coeval.now(bb.getString)
          body <- aux
        } yield LET(name, body)
      case DEC_FUNC =>
        for {
          name <- Coeval.now(bb.getString)
          args <- {
            val argsCnt = bb.getInt
            if (argsCnt <= (bb.limit() - bb.position()) / 2 && argsCnt >= 0) {
              Coeval.now(for (_ <- 1 to argsCnt) yield bb.getString)
            } else {
              Coeval.raiseError(new Exception(s"At position ${bb.position()} array of arguments names too big."))
            }
          }
          body <- aux
        } yield FUNC(name, args.toList, body)
    }
  }

  def desAux(bb: ByteBuffer, allowObjects: Boolean = false, acc: Coeval[Unit] = Coeval.now(())): Coeval[EXPR] =
    desAuxR(bb, allowObjects, acc)

  private def desAuxR(bb: ByteBuffer, allowObjects: Boolean, acc: Coeval[Unit]): Coeval[EXPR] = acc.flatMap { _ =>
    (bb.get(): @unchecked) match {
      case E_LONG   => Coeval.now(CONST_LONG(bb.getLong))
      case E_BYTES  => Coeval.now(CONST_BYTESTR(ByteStr(bb.getBytes)).explicitGet())
      case E_STRING => Coeval.now(CONST_STRING(bb.getString).explicitGet())
      case E_IF     => (desAuxR(bb, allowObjects, acc), desAuxR(bb, allowObjects, acc), desAuxR(bb, allowObjects, acc)).mapN(IF)
      case E_BLOCK =>
        for {
          name     <- Coeval.now(bb.getString)
          letValue <- desAuxR(bb, allowObjects, acc)
          body     <- desAuxR(bb, allowObjects, acc)
        } yield LET_BLOCK(
          let = LET(name, letValue),
          body = body
        )
      case E_BLOCK_V2 =>
        for {
          decType <- Coeval.now(bb.get())
          dec     <- deserializeDeclaration(bb, desAuxR(bb, allowObjects, acc), decType)
          body    <- desAuxR(bb, allowObjects, acc)
        } yield BLOCK(dec, body)
      case E_REF    => Coeval.now(REF(bb.getString))
      case E_TRUE   => Coeval.now(TRUE)
      case E_FALSE  => Coeval.now(FALSE)
      case E_GETTER => desAuxR(bb, allowObjects, acc).map(GETTER(_, field = bb.getString))
      case E_FUNCALL =>
        Coeval
          .now((bb.getFunctionHeader, bb.getInt))
          .flatMap {
            case (header, argc) =>
              if (argc <= (bb.limit() - bb.position()) && argc >= 0) {
                val args: List[Coeval[EXPR]] = (1 to argc).map(_ => desAuxR(bb, allowObjects, acc)).toList
                args.sequence[Coeval, EXPR].map(FUNCTION_CALL(header, _))
              } else {
                tooBigArray(bb)
              }
          }
      case E_ARR =>
        Coeval
          .now(bb.getInt)
          .flatMap(
            argsCount =>
              if (argsCount <= (bb.limit() - bb.position()) && argsCount >= 0)
                (1 to argsCount)
                  .to(LazyList)
                  .traverse(_ => evaluatedOnly(desAuxR(bb, allowObjects, acc)))
                  .map(elements => ARR(elements.toIndexedSeq, limited = false).explicitGet())
              else
                tooBigArray(bb)
          )
      case E_CASE_OBJ if allowObjects =>
        for {
          (typeName, fieldsNumber) <- Coeval((bb.getString, bb.getInt))
          fields <- (1 to fieldsNumber)
            .to(LazyList)
            .traverse(
              _ =>
                for {
                  fieldName  <- Coeval.now(bb.getString)
                  fieldValue <- evaluatedOnly(desAuxR(bb, allowObjects, acc))
                } yield (fieldName, fieldValue)
            )
        } yield CaseObj(CASETYPEREF(typeName, Nil), fields.toMap)
    }
  }

  private def evaluatedOnly(arg: Coeval[EXPR]): Coeval[EVALUATED] =
    arg.flatMap {
      case value: EVALUATED => Coeval.now(value)
      case other            => Coeval.raiseError(new Exception(s"Unsupported array element: $other"))
    }

  private def tooBigArray(bb: ByteBuffer) = {
    Coeval.raiseError(new Exception(s"At position ${bb.position()} array of arguments too big."))
  }

  def deserialize(bytes: Array[Byte], all: Boolean = true, allowObjects: Boolean = false): Either[String, (EXPR, Int)] = {
    val bb = ByteBuffer.wrap(bytes)
    val res = Try(desAux(bb, allowObjects).value()).toEither.left
      .map(_.getMessage)
    (if (all)
       res.flatMap { r =>
         if (bb.hasRemaining) Left(s"${bb.remaining()} bytes left")
         else Right(r)
       } else res)
      .map((_, bb.remaining()))
  }

  def deserialize(bb: ByteBuffer): Either[String, EXPR] =
    Try(desAux(bb).value()).toEither.left.map(_.getMessage)

  def deserializeFunctionCall(bb: ByteBuffer): Either[Throwable, FUNCTION_CALL] =
    Try(desAux(bb).value()).toEither.flatMap {
      case fc: FUNCTION_CALL => Right(fc)
      case other => Left(new RuntimeException(s"Not a function call: $other"))
    }

  def deserializeFunctionCall(bb: Array[Byte]): Either[Throwable, FUNCTION_CALL] =
    deserializeFunctionCall(ByteBuffer.wrap(bb))

  def serAux(out: ByteArrayOutputStream, acc: Coeval[Unit], expr: EXPR, allowObjects: Boolean = false): Coeval[Unit] = acc.flatMap { _ =>
    expr match {
      case CONST_LONG(n) =>
        Coeval.now {
          out.write(E_LONG)
          out.writeLong(n)
        }
      case CONST_BYTESTR(bs) =>
        Coeval.now {
          out.write(E_BYTES)
          out.writeInt(Math.toIntExact(bs.arr.length)).write(bs.arr)
        }
      case CONST_STRING(s) =>
        Coeval.now {
          out.write(E_STRING)
          out.writeString(s)
        }
      case IF(cond, ifTrue, ifFalse) =>
        List(cond, ifTrue, ifFalse).foldLeft(Coeval.now(out.write(E_IF)))((acc, expr) => serAux(out, acc, expr, allowObjects))
      case LET_BLOCK(LET(name, value), body) =>
        val n = Coeval.now[Unit] {
          out.write(E_BLOCK)
          out.writeString(name)
        }
        List(value, body).foldLeft(n)((acc, expr) => serAux(out, acc, expr, allowObjects))
      case BLOCK(dec, body) =>
        val n = Coeval.now[Unit] {
          out.write(E_BLOCK_V2)
        }
        serAux(out, serializeDeclaration(out, dec, serAux(out, n, _, allowObjects)), body, allowObjects)
      case REF(key) =>
        Coeval.now {
          out.write(E_REF)
          out.writeString(key)
        }
      case CONST_BOOLEAN(b) =>
        Coeval.now(
          out.write(
            if (b)
              E_TRUE
            else
              E_FALSE
          )
        )
      case GETTER(obj, field) =>
        serAux(out, Coeval.now[Unit](out.write(E_GETTER)), obj, allowObjects).map { _ =>
          out.writeString(field)
        }
      case FUNCTION_CALL(header, args) =>
        val n = Coeval.now[Unit] {
          out.write(E_FUNCALL)
          out.writeFunctionHeader(header)
          out.writeInt(args.size)
        }
        args.foldLeft(n)((acc, arg) => serAux(out, acc, arg, allowObjects))

      case ARR(elements) =>
        val dataInfo = Coeval.now[Unit] {
          out.write(E_ARR)
          out.writeInt(elements.size)
        }
        elements.foldLeft(dataInfo)((acc, element) => serAux(out, acc, element, allowObjects))

      case CaseObj(caseType, fields) if allowObjects =>
        val dataInfo = Coeval.now[Unit] {
          out.write(E_CASE_OBJ)
          out.writeString(caseType.name)
          out.writeInt(fields.size)
        }
        fields.foldLeft(dataInfo) {
          case (acc, (fieldName, fieldValue)) =>
            for {
              _ <- Coeval.now(out.writeString(fieldName))
              r <- serAux(out, acc, fieldValue, allowObjects)
            } yield r
        }

      case x =>
        Coeval.raiseError(new Exception(s"Serialization of value $x is unsupported"))
    }
  }

  def serAuxOptimized(out: CodedOutputStream, acc: Coeval[Unit], expr: EXPR, allowObjects: Boolean = false): Coeval[Unit] = acc.flatMap { _ =>
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
        List(cond, ifTrue, ifFalse).foldLeft(Coeval.now(out.writeRawByte(E_IF)))((acc, expr) => serAuxOptimized(out, acc, expr, allowObjects))
      case LET_BLOCK(LET(name, value), body) =>
        val n = Coeval.now[Unit] {
          out.writeRawByte(E_BLOCK)
          out.writeStringNoTag(name)
        }
        List(value, body).foldLeft(n)((acc, expr) => serAuxOptimized(out, acc, expr, allowObjects))
      case BLOCK(dec, body) =>
        val n = Coeval.now[Unit] {
          out.writeRawByte(E_BLOCK_V2)
        }
        serAuxOptimized(out, serializeDeclarationOptimized(out, dec, serAuxOptimized(out, n, _, allowObjects)), body, allowObjects)
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
        serAuxOptimized(out, Coeval.now[Unit](out.writeRawByte(E_GETTER)), obj, allowObjects).map { _ =>
          out.writeStringNoTag(field)
        }
      case FUNCTION_CALL(header, args) =>
        val n = Coeval.now[Unit] {
          out.writeRawByte(E_FUNCALL)
          out.writeFunctionHeaderOptimized(header)
          out.writeRawByte(args.size.toByte)
        }
        args.foldLeft(n)((acc, arg) => serAuxOptimized(out, acc, arg, allowObjects))

      case ARR(elements) =>
        val dataInfo = Coeval.now[Unit] {
          out.writeRawByte(E_ARR)
          out.writeUInt32NoTag(elements.size)
        }
        elements.foldLeft(dataInfo)((acc, element) => serAuxOptimized(out, acc, element, allowObjects))

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
              r <- serAuxOptimized(out, acc, fieldValue, allowObjects)
            } yield r
        }

      case x =>
        Coeval.raiseError(new Exception(s"Serialization of value $x is unsupported"))
    }
  }

  def serialize(expr: EXPR, allowObjects: Boolean = false): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    serAux(out, acc = Coeval.now(()), expr, allowObjects).value()
    out.toByteArray
  }
}

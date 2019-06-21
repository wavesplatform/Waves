package com.wavesplatform.lang.v1

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.utils.Serialize._
import monix.eval.Coeval

import scala.util.Try

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

  val E_BLOCK_V2 = 10

  val DEC_LET  = 0
  val DEC_FUNC = 1

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
    }
  }

  def deserializeDeclaration(bb: ByteBuffer, aux: => Coeval[EXPR], decType: Byte): Coeval[DECLARATION] = {
    decType match {
      case DEC_LET =>
        for {
          name <- Coeval.now(bb.getString)
          body <- aux
        } yield LET(name, body)
      case DEC_FUNC =>
        for {
          name <- Coeval.now(bb.getString)
          args <- ({
            val argsCnt = bb.getInt
            if (argsCnt <= (bb.limit() - bb.position())/2 && argsCnt >= 0) {
              Coeval.now(for (_ <- 1 to argsCnt) yield bb.getString)
            } else {
              Coeval.raiseError(new Exception(s"At position ${bb.position()} array of arguments names too big."))
            }
          })
          body <- aux
        } yield FUNC(name, args.toList, body)
    }
  }

  def desAux(bb: ByteBuffer, acc: Coeval[Unit] = Coeval.now(())): Coeval[EXPR] = acc.flatMap { _ =>
    bb.get() match {
      case E_LONG   => Coeval.now(CONST_LONG(bb.getLong))
      case E_BYTES  => Coeval.now(CONST_BYTESTR(ByteStr(bb.getBytes)).explicitGet())
      case E_STRING => Coeval.now(CONST_STRING(bb.getString).explicitGet())
      case E_IF     => (desAux(bb), desAux(bb), desAux(bb)).mapN(IF)
      case E_BLOCK =>
        for {
          name     <- Coeval.now(bb.getString)
          letValue <- desAux(bb)
          body     <- desAux(bb)
        } yield
          LET_BLOCK(
            let = LET(name, letValue),
            body = body
          )
      case E_BLOCK_V2 =>
        for {
          decType <- Coeval.now(bb.get())
          dec     <- deserializeDeclaration(bb, desAux(bb), decType)
          body    <- desAux(bb)
        } yield BLOCK(dec, body)
      case E_REF    => Coeval.now(REF(bb.getString))
      case E_TRUE   => Coeval.now(TRUE)
      case E_FALSE  => Coeval.now(FALSE)
      case E_GETTER => desAux(bb).map(GETTER(_, field = bb.getString))
      case E_FUNCALL =>
        Coeval
          .now((bb.getFunctionHeader, bb.getInt))
          .flatMap {
            case (header, argc) =>
              if (argc <= (bb.limit() - bb.position()) && argc >= 0) {
                val args: List[Coeval[EXPR]] = (1 to argc).map(_ => desAux(bb))(collection.breakOut)
                args.sequence[Coeval, EXPR].map(FUNCTION_CALL(header, _))
              } else {
                Coeval.raiseError(new Exception(s"At position ${bb.position()} array of arguments too big."))
              }
          }
    }
  }

  def deserialize(bytes: Array[Byte], all: Boolean = true): Either[String, (EXPR, Int)] = {
    val bb = ByteBuffer.wrap(bytes)
    val res = Try(desAux(bb).value).toEither.left
      .map(_.getMessage)
    (if (all)
       res.flatMap { r =>
         if (bb.hasRemaining) Left(s"${bb.remaining()} bytes left")
         else Right(r)
       } else res)
      .map((_, bb.remaining()))
  }

  def serAux(out: ByteArrayOutputStream, acc: Coeval[Unit], expr: EXPR): Coeval[Unit] = acc.flatMap { _ =>
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
        List(cond, ifTrue, ifFalse).foldLeft(Coeval.now(out.write(E_IF)))((acc, expr) => serAux(out, acc, expr))
      case LET_BLOCK(LET(name, value), body) =>
        val n = Coeval.now[Unit] {
          out.write(E_BLOCK)
          out.writeString(name)
        }
        List(value, body).foldLeft(n)((acc, expr) => serAux(out, acc, expr))
      case BLOCK(dec, body) =>
        val n = Coeval.now[Unit] {
          out.write(E_BLOCK_V2)
        }
        serAux(out, serializeDeclaration(out, dec, serAux(out, n, _)), body)
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
        serAux(out, Coeval.now[Unit](out.write(E_GETTER)), obj).map { _ =>
          out.writeString(field)
        }
      case FUNCTION_CALL(header, args) =>
        val n = Coeval.now[Unit] {
          out.write(E_FUNCALL)
          out.writeFunctionHeader(header)
          out.writeInt(args.size)
        }
        args.foldLeft(n)((acc, arg) => serAux(out, acc, arg))
      case x => println(x); ??? //TODO: FIx exhaustivness
    }
  }

  def serialize(expr: EXPR): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    serAux(out, acc = Coeval.now(()), expr).value
    out.toByteArray
  }
}

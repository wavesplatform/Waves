package com.wavesplatform.lang.v1

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval
import scodec.bits.ByteVector

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

  def deserialize(bytes: Array[Byte]): Either[String, EXPR] = {
    import cats.instances.list._
    import cats.syntax.apply._
    import cats.syntax.traverse._

    val bb = ByteBuffer.wrap(bytes)
    def aux(acc: Coeval[Unit] = Coeval.now(())): Coeval[EXPR] = acc.flatMap { _ =>
      bb.get() match {
        case E_LONG   => Coeval.now(CONST_LONG(bb.getLong))
        case E_BYTES  => Coeval.now(CONST_BYTEVECTOR(bb.getByteVector))
        case E_STRING => Coeval.now(CONST_STRING(bb.getString))
        case E_IF     => (aux(), aux(), aux()).mapN(IF)
        case E_BLOCK =>
          for {
            name     <- Coeval.now(bb.getString)
            letValue <- aux()
            body     <- aux()
          } yield
            BLOCK(
              let = LET(name, letValue),
              body = body
            )
        case E_REF    => Coeval.now(REF(bb.getString))
        case E_TRUE   => Coeval.now(TRUE)
        case E_FALSE  => Coeval.now(FALSE)
        case E_GETTER => aux().map(GETTER(_, field = bb.getString))
        case E_FUNCALL =>
          Coeval
            .now((bb.getFunctionHeader, bb.getInt))
            .flatMap {
              case (header, argc) =>
                val args: List[Coeval[EXPR]] = (1 to argc).map(_ => aux())(collection.breakOut)
                args.sequence[Coeval, EXPR].map(FUNCTION_CALL(header, _))
            }
      }
    }

    Try(aux().value).toEither.left
      .map(_.getMessage)
      .flatMap { r =>
        if (bb.hasRemaining) Left(s"${bb.remaining()} bytes left")
        else Right(r)
      }
  }

  implicit class ByteBufferOps(val self: ByteBuffer) extends AnyVal {
    def getBytes: Array[Byte] = {
      val len   = self.getInt
      val bytes = new Array[Byte](len)
      self.get(bytes)
      bytes
    }

    def getByteVector: ByteVector = ByteVector(getBytes)

    def getString: String = new String(getBytes, StandardCharsets.UTF_8)

    def getFunctionHeader: FunctionHeader = self.get() match {
      case FH_NATIVE => FunctionHeader.Native(self.getShort)
      case FH_USER   => FunctionHeader.User(getString)
      case x         => throw new RuntimeException(s"Unknown function header type: $x")
    }
  }

  implicit class ByteArrayOutputStreamOps(val self: ByteArrayOutputStream) extends AnyVal {
    def writeShort(value: Short): ByteArrayOutputStream = writeNumber(value, 2)
    def writeInt(value: Int): ByteArrayOutputStream     = writeNumber(value, 4)
    def writeLong(value: Long): ByteArrayOutputStream   = writeNumber(value, 8)

    def writeNumber(n: Long, byteCount: Int): ByteArrayOutputStream = {
      (byteCount - 1 to 0 by -1).foreach { i =>
        self.write((n >> (8 * i) & 0xffL).toInt)
      }
      self
    }

    def writeString(x: String): ByteArrayOutputStream = {
      val bytes = x.getBytes(StandardCharsets.UTF_8)
      self.writeInt(bytes.length)
      self.write(bytes)
      self
    }

    def writeFunctionHeader(h: FunctionHeader): ByteArrayOutputStream = h match {
      case FunctionHeader.Native(id) =>
        self.write(FH_NATIVE)
        self.writeShort(id)
      case FunctionHeader.User(name) =>
        self.write(FH_USER)
        self.writeString(name)
    }
  }

  def serialize(expr: EXPR): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    def aux(acc: Coeval[Unit], expr: EXPR): Coeval[Unit] = acc.flatMap { _ =>
      expr match {
        case CONST_LONG(n) =>
          Coeval.now {
            out.write(E_LONG)
            out.writeLong(n)
          }
        case CONST_BYTEVECTOR(bs) =>
          Coeval.now {
            out.write(E_BYTES)
            out.writeInt(Math.toIntExact(bs.size)).write(bs.toArray)
          }
        case CONST_STRING(s) =>
          Coeval.now {
            out.write(E_STRING)
            out.writeString(s)
          }
        case IF(cond, ifTrue, ifFalse) =>
          List(cond, ifTrue, ifFalse).foldLeft(Coeval.now(out.write(E_IF)))(aux)
        case BLOCK(LET(name, value), body) =>
          val n = Coeval.now[Unit] {
            out.write(E_BLOCK)
            out.writeString(name)
          }
          List(value, body).foldLeft(n)(aux)
        case REF(key) =>
          Coeval.now {
            out.write(E_REF)
            out.writeString(key)
          }
        case TRUE  => Coeval.now(out.write(E_TRUE))
        case FALSE => Coeval.now(out.write(E_FALSE))
        case GETTER(obj, field) =>
          aux(Coeval.now[Unit](out.write(E_GETTER)), obj).map { _ =>
            out.writeString(field)
          }
        case FUNCTION_CALL(header, args) =>
          val n = Coeval.now[Unit] {
            out.write(E_FUNCALL)
            out.writeFunctionHeader(header)
            out.writeInt(args.size)
          }
          args.foldLeft(n)(aux)
      }
    }

    aux(acc = Coeval.now(()), expr).value
    out.toByteArray
  }
}

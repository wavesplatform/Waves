package com.wavesplatform.lang.v1

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.wavesplatform.lang.v1.compiler.Terms._
import monix.eval.Coeval
import scodec._
import scodec.bits.ByteVector
import scodec.codecs._

import scala.util.Try

object Serde {

  import codecs.implicits._

  implicit val dFunctionHeader       = Discriminated[FunctionHeader, Int](uint8)
  implicit val dFunctionHeaderSystem = dFunctionHeader.bind[FunctionHeader.Native](0)
  implicit val dFunctionHeaderUser   = dFunctionHeader.bind[FunctionHeader.User](1)

  implicit val d                = Discriminated[EXPR, Int](uint8)
  implicit val dConstInt        = d.bind[CONST_LONG](0)
  implicit val dConstByteVector = d.bind[CONST_BYTEVECTOR](1)
  implicit val dConstString     = d.bind[CONST_STRING](2)
  implicit val dIf              = d.bind[IF](3)
  implicit val dComposite       = d.bind[BLOCK](4)
  implicit val dRef             = d.bind[REF](5)
  implicit val dTrue            = d.bind[TRUE.type](6)
  implicit val dFalse           = d.bind[FALSE.type](7)
  implicit val dGetter          = d.bind[GETTER](8)
  implicit val dFunctionCall    = d.bind[FUNCTION_CALL](9)

  val codec: Codec[EXPR] = Codec[EXPR]

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

  val FH_NATIVE: Byte = 0 ///naming
  val FH_USER: Byte   = 1

  def deserialize(bb: ByteBuffer): Either[String, EXPR] = {
    import cats.instances.list._
    import cats.syntax.apply._
    import cats.syntax.traverse._

    def aux: Coeval[EXPR] = bb.get() match {
      case E_LONG   => Coeval.now(CONST_LONG(bb.getLong))
      case E_BYTES  => Coeval.now(CONST_BYTEVECTOR(bb.getByteVector))
      case E_STRING => Coeval.now(CONST_STRING(bb.getString))
      case E_IF     => (aux, aux, aux).mapN(IF)
      case E_BLOCK =>
        val name = bb.getString
        for {
          letValue <- aux
          body     <- aux
        } yield
          BLOCK(
            let = LET(name, letValue),
            body = body
          )
      case E_REF    => Coeval.now(REF(bb.getString))
      case E_TRUE   => Coeval.now(TRUE)
      case E_FALSE  => Coeval.now(FALSE)
      case E_GETTER => aux.map(GETTER(_, field = bb.getString))
      case E_FUNCALL =>
        val header = bb.getFunctionHeader

        val args: List[Coeval[EXPR]] = (1 to bb.getInt).map(_ => aux)(collection.breakOut)
        args.sequence[Coeval, EXPR].map(FUNCTION_CALL(header, _))
    }

    Try(aux.value).toEither.left
      .map(_.getMessage)
      .flatMap { r =>
        if (bb.hasRemaining) Left(s"${bb.remaining()} bytes left")
        else Right(r)
      }
  }

  implicit class ByteBufferOps(val self: ByteBuffer) extends AnyVal {
    def getByteVector: ByteVector = {
      val len   = self.getLong()
      val bytes = new Array[Byte](Math.toIntExact(len))
      self.get(bytes)
      ByteVector(bytes)
    }

    def getBytes: Array[Byte] = {
      val len   = self.getInt()
      val bytes = new Array[Byte](len)
      self.get(bytes)
      bytes
    }

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

  def serialize(expr: EXPR, bb: ByteArrayOutputStream): ByteArrayOutputStream = {
    val done: Coeval[Unit] = Coeval.now(())

    def aux(acc: Coeval[Unit], expr: EXPR): Coeval[Unit] = acc.flatMap { _ =>
      expr match {
        case CONST_LONG(n) =>
          Coeval.now {
            bb.write(E_LONG)
            bb.writeLong(n)
          }
        case CONST_BYTEVECTOR(bs) =>
          Coeval.now {
            bb.write(E_BYTES)
            bb.writeLong(bs.size).write(bs.toArray) /// array size long -> short?
          }
        case CONST_STRING(s) =>
          Coeval.now {
            bb.write(E_STRING)
            bb.writeString(s)
          }
        case IF(cond, ifTrue, ifFalse) =>
          bb.write(E_IF)
          List(cond, ifTrue, ifFalse).foldLeft(done)(aux)
        case BLOCK(LET(name, value), body) =>
          bb.write(E_BLOCK)
          bb.writeString(name)
          List(value, body).foldLeft(done)(aux)
        case REF(key) =>
          Coeval.now {
            bb.write(E_REF)
            bb.writeString(key)
          }
        case TRUE  => Coeval.now(bb.write(E_TRUE))
        case FALSE => Coeval.now(bb.write(E_FALSE))
        case GETTER(obj, field) =>
          bb.write(E_GETTER)
          aux(done, obj).map { _ =>
            bb.writeString(field)
          }
        case FUNCTION_CALL(header, args) =>
          bb.write(E_FUNCALL)
          bb.writeFunctionHeader(header)
          bb.writeInt(args.size)
          args.foldLeft(done)(aux)
      }
    }

    aux(acc = Coeval.now(()), expr).value
    bb
  }
}

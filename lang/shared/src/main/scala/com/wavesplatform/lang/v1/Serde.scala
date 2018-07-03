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

  val C_LONG: Byte    = 0
  val C_BYTES: Byte   = 1
  val C_STRING: Byte  = 2
  val C_IF: Byte      = 3
  val C_BLOCK: Byte   = 4
  val C_REF: Byte     = 5
  val C_TRUE: Byte    = 6
  val C_FALSE: Byte   = 7
  val C_GETTER: Byte  = 8
  val C_FUNCALL: Byte = 9

  val H_NATIVE: Byte = 0 ///naming
  val H_USER: Byte   = 1

  def deserialize(bb: ByteBuffer): Either[String, EXPR] = {
    import cats.instances.list._
    import cats.syntax.apply._
    import cats.syntax.traverse._

    def aux: Coeval[EXPR] = bb.get() match {
      case C_LONG   => Coeval.now(CONST_LONG(bb.getLong))
      case C_BYTES  => Coeval.now(CONST_BYTEVECTOR(bb.getByteVector))
      case C_STRING => Coeval.now(CONST_STRING(bb.getString))
      case C_IF     => (aux, aux, aux).mapN(IF)
      case C_BLOCK =>
        val name = bb.getString
        for {
          letValue <- aux
          body     <- aux
        } yield
          BLOCK(
            let = LET(name, letValue),
            body = body
          )
      case C_REF    => Coeval.now(REF(bb.getString))
      case C_TRUE   => Coeval.now(TRUE)
      case C_FALSE  => Coeval.now(FALSE)
      case C_GETTER => aux.map(GETTER(_, field = bb.getString))
      case C_FUNCALL =>
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
      case H_NATIVE => FunctionHeader.Native(self.getShort)
      case H_USER   => FunctionHeader.User(getString)
      case x        => throw new RuntimeException(s"Unknown function header type: $x")
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
        self.write(H_NATIVE)
        self.writeShort(id)
      case FunctionHeader.User(name) =>
        self.write(H_USER)
        self.writeString(name)
    }
  }

  def serialize(expr: EXPR, bb: ByteArrayOutputStream): ByteArrayOutputStream = expr match {
    case CONST_LONG(n) =>
      bb.write(C_LONG)
      bb.writeLong(n)
    case CONST_BYTEVECTOR(bs) =>
      bb.write(C_BYTES)
      bb.writeLong(bs.size).write(bs.toArray) /// array size long -> short?
      bb
    case CONST_STRING(s) =>
      bb.write(C_STRING)
      bb.writeString(s)
    case IF(cond, ifTrue, ifFalse) =>
      bb.write(C_IF)
      serialize(cond, bb)
      serialize(ifTrue, bb)
      serialize(ifFalse, bb)
    case BLOCK(LET(name, value), body) =>
      bb.write(C_BLOCK)
      bb.writeString(name)
      serialize(value, bb)
      serialize(body, bb)
    case REF(key) =>
      bb.write(C_REF)
      bb.writeString(key)
    case TRUE =>
      bb.write(C_TRUE)
      bb
    case FALSE =>
      bb.write(C_FALSE)
      bb
    case GETTER(obj, field) =>
      bb.write(C_GETTER)
      serialize(obj, bb)
      bb.writeString(field)
    case FUNCTION_CALL(header, args) =>
      bb.write(C_FUNCALL)
      bb.writeFunctionHeader(header)
      bb.writeInt(args.size)
      args.foreach(serialize(_, bb))
      bb
  }
}

package com.wavesplatform.lang.v1

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

  def deserialize(bb: ByteBuffer): Either[String, EXPR] = {
    import cats.instances.list._
    import cats.syntax.apply._
    import cats.syntax.traverse._

    def aux: Coeval[EXPR] = bb.get() match {
      case 0 => Coeval.now(CONST_LONG(bb.getLong))
      case 1 => Coeval.now(CONST_BYTEVECTOR(bb.getByteVector))
      case 2 => Coeval.now(CONST_STRING(bb.getString))
      case 3 => (aux, aux, aux).mapN(IF)
      case 4 =>
        val name = bb.getString
        for {
          letValue <- aux
          body     <- aux
        } yield
          BLOCK(
            let = LET(name, letValue),
            body = body
          )
      case 5 => Coeval.now(REF(bb.getString))
      case 6 => Coeval.now(TRUE)
      case 7 => Coeval.now(FALSE)
      case 8 => aux.map(GETTER(_, field = bb.getString))
      case 9 =>
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
      case 0 => FunctionHeader.Native(self.getShort)
      case 1 => FunctionHeader.User(getString)
      case x => throw new RuntimeException(s"Unknown function header type: $x")
    }
  }

}

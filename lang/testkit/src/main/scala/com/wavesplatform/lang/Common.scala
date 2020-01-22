package com.wavesplatform.lang

import cats.Id
import cats.kernel.Monoid
import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.{Contextful, EvaluatorV1}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext, _}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import monix.eval.Coeval
import org.scalacheck.Shrink

import scala.util.{Left, Right, Try}

object Common {
  import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters._

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTESTR, STRING)
  val dataEntryType              = CASETYPEREF("DataEntry", List("key" -> STRING, "value" -> dataEntryValueType))
  val addCtx: CTX[NoContext]            = CTX[NoContext](Seq(dataEntryType), Map.empty, Array.empty)

  def ev[T <: EVALUATED](context: EvaluationContext[NoContext, Id] = Monoid.combine(PureContext.build(Global, V1).evaluationContext, addCtx.evaluationContext),
                         expr: EXPR): Either[ExecutionError, T] =
    new EvaluatorV1[Id, NoContext]().apply[T](context, expr)

  trait NoShrink {
    implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  val multiplierFunction: NativeFunction[NoContext] =
    NativeFunction("MULTIPLY", 1L, 10005.toShort, LONG, ("x1", LONG), ("x2", LONG)) {
      case CONST_LONG(x1: Long) :: CONST_LONG(x2: Long) :: Nil => Try(x1 * x2).map(CONST_LONG).toEither.left.map(_.toString)
      case _                                                   => ??? // suppress pattern match warning
    }

  val pointTypeA = CASETYPEREF("PointA", List("X"  -> LONG, "YA" -> LONG))
  val pointTypeB = CASETYPEREF("PointB", List("X"  -> LONG, "YB" -> LONG))
  val pointTypeC = CASETYPEREF("PointC", List("YB" -> LONG))
  val pointTypeD = CASETYPEREF("PointD", List("YB" -> UNION(LONG, UNIT)))

  val AorB    = UNION(pointTypeA, pointTypeB)
  val AorBorC = UNION(pointTypeA, pointTypeB, pointTypeC)
  val BorC    = UNION(pointTypeB, pointTypeC)
  val CorD    = UNION(pointTypeC, pointTypeD)

  val pointAInstance  = CaseObj(pointTypeA, Map("X"  -> 3L, "YA" -> 40L))
  val pointBInstance  = CaseObj(pointTypeB, Map("X"  -> 3L, "YB" -> 41L))
  val pointCInstance  = CaseObj(pointTypeC, Map("YB" -> 42L))
  val pointDInstance1 = CaseObj(pointTypeD, Map("YB" -> 43L))

  val pointDInstance2 = CaseObj(pointTypeD, Map("YB" -> unit))

  val sampleTypes = Seq(pointTypeA, pointTypeB, pointTypeC, pointTypeD) ++ Seq(UNION.create(AorB.typeList, Some("PointAB")),
                                                                               UNION.create(BorC.typeList, Some("PointBC")),
                                                                               UNION.create(CorD.typeList, Some("PointCD")))

  def sampleUnionContext(instance: CaseObj) =
    EvaluationContext.build(
      Map.empty,
      Map("p" -> LazyVal.fromEvaluated[Id](instance)),
      Seq.empty[BaseFunction[NoContext]]
    )

  def emptyBlockchainEnvironment(h: Int = 1, in: Coeval[Environment.InputEntity] = Coeval(???), nByte: Byte = 'T'): Environment[Id] = new Environment[Id] {
    override def height: Long  = h
    override def chainId: Byte = nByte
    override def inputEntity   = in()

    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
    override def transferTransactionById(id: Array[Byte]): Option[Tx]                                            = ???
    override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
    override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo]                                         = ???
    override def lastBlockOpt(): Option[BlockInfo]                                                               = ???
    override def blockInfoByHeight(height: Int): Option[BlockInfo]                                               = ???
    override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any]                        = ???
    override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
    override def tthis: Recipient.Address                                                                        = ???
  }

  def addressFromPublicKey(chainId: Byte, pk: Array[Byte], addressVersion: Byte = EnvironmentFunctions.AddressVersion): Array[Byte] = {
    val publicKeyHash   = Global.secureHash(pk).take(EnvironmentFunctions.HashLength)
    val withoutChecksum = addressVersion +: chainId +: publicKeyHash
    withoutChecksum ++ Global.secureHash(withoutChecksum).take(EnvironmentFunctions.ChecksumLength)
  }

  def addressFromString(chainId: Byte, str: String): Either[String, Option[Array[Byte]]] = {
    val base58String = if (str.startsWith(EnvironmentFunctions.AddressPrefix)) str.drop(EnvironmentFunctions.AddressPrefix.length) else str
    Global.base58Decode(base58String, Global.MaxAddressLength) match {
      case Left(e) => Left(e)
      case Right(addressBytes) =>
        val version = addressBytes.head
        val network = addressBytes.tail.head
        lazy val checksumCorrect = {
          val checkSum = addressBytes.takeRight(EnvironmentFunctions.ChecksumLength)
          val checkSumGenerated =
            Global.secureHash(addressBytes.dropRight(EnvironmentFunctions.ChecksumLength)).take(EnvironmentFunctions.ChecksumLength)
          checkSum sameElements checkSumGenerated
        }

        if (version == EnvironmentFunctions.AddressVersion && network == chainId && addressBytes.length == EnvironmentFunctions.AddressLength && checksumCorrect)
          Right(Some(addressBytes))
        else Right(None)
    }
  }
}

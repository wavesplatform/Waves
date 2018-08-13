package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext}
import com.wavesplatform.lang.v1.traits.domain.{Ord, Recipient, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import monix.eval.Coeval
import org.scalacheck.Shrink
import org.scalatest.matchers.{MatchResult, Matcher}
import shapeless.{:+:, CNil}

import scala.util.{Left, Right, Try}

object Common {

  def ev[T](context: EvaluationContext = PureContext.evalContext, expr: EXPR): (EvaluationContext, Either[ExecutionError, T]) =
    EvaluatorV1[T](context, expr)

  trait NoShrink {
    implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  }

  class ProduceError(errorMessage: String) extends Matcher[Either[_, _]] {
    override def apply(ei: Either[_, _]): MatchResult = {
      ei match {
        case r @ Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
        case l @ Left(_) =>
          MatchResult(matches = l.toString contains errorMessage,
                      "expecting Left(...{0}...) but got {1}",
                      "got expected error",
                      IndexedSeq(errorMessage, l))
      }
    }
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  val multiplierFunction: NativeFunction =
    NativeFunction("MULTIPLY", 1, 10005, LONG, "x1" -> LONG, "x2" -> LONG) {
      case (x1: Long) :: (x2: Long) :: Nil => Try(x1 * x2).toEither.left.map(_.toString)
      case _                               => ??? // suppress pattern match warning
    }

  val pointTypeA = CaseType("PointA", List("X"  -> LONG, "YA" -> LONG))
  val pointTypeB = CaseType("PointB", List("X"  -> LONG, "YB" -> LONG))
  val pointTypeC = CaseType("PointC", List("YB" -> LONG))
  val pointTypeD = CaseType("PointD", List("YB" -> UNION(LONG, UNIT)))

  val AorB    = UNION(pointTypeA.typeRef, pointTypeB.typeRef)
  val AorBorC = UNION(pointTypeA.typeRef, pointTypeB.typeRef, pointTypeC.typeRef)
  val BorC    = UNION(pointTypeB.typeRef, pointTypeC.typeRef)
  val CorD    = UNION(pointTypeC.typeRef, pointTypeD.typeRef)

  val pointAInstance     = CaseObj(pointTypeA.typeRef, Map("X" -> 3L, "YA" -> 40L))
  val pointBInstance     = CaseObj(pointTypeB.typeRef, Map("X" -> 3L, "YB" -> 41L))
  val pointCInstance     = CaseObj(pointTypeC.typeRef, Map("YB" -> 42L))
  val pointDInstance1    = CaseObj(pointTypeD.typeRef, Map("YB" -> 43L))
  private val unit: Unit = ()
  val pointDInstance2    = CaseObj(pointTypeD.typeRef, Map("YB" -> unit))

  val sampleTypes = Seq(pointTypeA, pointTypeB, pointTypeC, pointTypeD) ++ Seq(UnionType("PointAB", AorB.l),
                                                                               UnionType("PointBC", BorC.l),
                                                                               UnionType("PointCD", CorD.l))

  def sampleUnionContext(instance: CaseObj) =
    EvaluationContext.build(Map.empty, Map("p" -> LazyVal(EitherT.pure(instance))), Seq.empty)

  def emptyBlockchainEnvironment(h: Int = 1, in: Coeval[Tx :+: Ord :+: CNil] = Coeval(???), nByte: Byte = 'T'): Environment = new Environment {
    override def height: Int       = h
    override def networkByte: Byte = nByte
    override def inputEntity       = in()

    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
    override def transactionHeightById(id: Array[Byte]): Option[Int]                                             = ???
    override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any]                        = ???
    override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
  }

  def addressFromPublicKey(networkByte: Byte, pk: Array[Byte], addressVersion: Byte = EnvironmentFunctions.AddressVersion): Array[Byte] = {
    val publicKeyHash   = Global.secureHash(pk).take(EnvironmentFunctions.HashLength)
    val withoutChecksum = addressVersion +: networkByte +: publicKeyHash
    withoutChecksum ++ Global.secureHash(withoutChecksum).take(EnvironmentFunctions.ChecksumLength)
  }

  def addressFromString(networkByte: Byte, str: String): Either[String, Option[Array[Byte]]] = {
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

        if (version == EnvironmentFunctions.AddressVersion && network == networkByte && addressBytes.length == EnvironmentFunctions.AddressLength && checksumCorrect)
          Right(Some(addressBytes))
        else Right(None)
    }
  }
}

package com.wavesplatform.lang

import cats.Id
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1.*
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.*
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, Log}
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.annotation.tailrec
import scala.util.{Left, Right, Try}

object Common {
  import com.wavesplatform.lang.v1.evaluator.ctx.impl.converters.*

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTESTR, STRING)
  val dataEntryType              = CASETYPEREF("DataEntry", List("key" -> STRING, "value" -> dataEntryValueType))
  val addCtx: CTX[NoContext]     = CTX[NoContext](Seq(dataEntryType), Map.empty, Array.empty)

  def ev[T <: EVALUATED](
      context: EvaluationContext[NoContext, Id] =
        Monoid.combine(PureContext.build(V1, useNewPowPrecision = true).evaluationContext, addCtx.evaluationContext),
      expr: EXPR
  ): Either[ExecutionError, T] =
    new EvaluatorV1[Id, NoContext]().apply[T](context, expr)

  val multiplierFunction: NativeFunction[NoContext] =
    NativeFunction("MULTIPLY", 1L, 10005.toShort, LONG, ("x1", LONG), ("x2", LONG)) {
      case CONST_LONG(x1: Long) :: CONST_LONG(x2: Long) :: Nil => Try(x1 * x2).map(CONST_LONG).toEither.left.map(_.toString)
      case _                                                   => ??? // suppress pattern match warning
    }

  val pointTypeA = CASETYPEREF("PointA", List("X" -> LONG, "YA" -> LONG))
  val pointTypeB = CASETYPEREF("PointB", List("X" -> LONG, "YB" -> LONG))
  val pointTypeC = CASETYPEREF("PointC", List("YB" -> LONG))
  val pointTypeD = CASETYPEREF("PointD", List("YB" -> UNION(LONG, UNIT)))

  val AorB    = UNION(pointTypeA, pointTypeB)
  val AorBorC = UNION(pointTypeA, pointTypeB, pointTypeC)
  val BorC    = UNION(pointTypeB, pointTypeC)
  val CorD    = UNION(pointTypeC, pointTypeD)

  val pointAInstance  = CaseObj(pointTypeA, Map("X" -> 3L, "YA" -> 40L))
  val pointBInstance  = CaseObj(pointTypeB, Map("X" -> 3L, "YB" -> 41L))
  val pointCInstance  = CaseObj(pointTypeC, Map("YB" -> 42L))
  val pointDInstance1 = CaseObj(pointTypeD, Map("YB" -> 43L))

  val pointDInstance2 = CaseObj(pointTypeD, Map("YB" -> unit))

  val sampleTypes = Seq(pointTypeA, pointTypeB, pointTypeC, pointTypeD) ++ Seq(
    UNION.create(AorB.typeList, Some("PointAB")),
    UNION.create(BorC.typeList, Some("PointBC")),
    UNION.create(CorD.typeList, Some("PointCD"))
  )

  def sampleUnionContext(instance: CaseObj) =
    EvaluationContext.build(
      Map.empty,
      Map("p" -> LazyVal.fromEvaluated[Id](instance)),
      Seq.empty[BaseFunction[NoContext]]
    )

  def emptyBlockchainEnvironment(h: Int = 1, in: Coeval[Environment.InputEntity] = Coeval(???), nByte: Byte = 'T'): Environment[Id] =
    new Environment[Id] {
      def height: Long  = h
      def chainId: Byte = nByte
      def inputEntity   = in()

      def transactionById(id: Array[Byte]): Option[Tx]                                    = ???
      def transferTransactionById(id: Array[Byte]): Option[Tx.Transfer]                   = ???
      def transactionHeightById(id: Array[Byte]): Option[Long]                            = ???
      def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo]                         = ???
      def lastBlockOpt(): Option[BlockInfo]                                               = ???
      def blockInfoByHeight(height: Int): Option[BlockInfo]                               = ???
      def data(recipient: Recipient, key: String, dataType: DataType): Option[Any]        = None
      def hasData(recipient: Recipient): Boolean                                          = false
      def resolveAlias(name: String): Either[String, Recipient.Address]                   = ???
      def accountBalanceOf(a: Recipient, b: Option[Array[Byte]]): Either[String, Long]    = ???
      def accountWavesBalanceOf(a: Recipient): Either[String, Environment.BalanceDetails] = ???
      def tthis: Environment.Tthis                                                        = Coproduct(Address(ByteStr.empty))
      def multiPaymentAllowed: Boolean                                                    = true
      def txId: ByteStr                                                                   = ???
      def transferTransactionFromProto(b: Array[Byte]): Option[Tx.Transfer]               = ???
      def addressFromString(address: String): Either[String, Recipient.Address]           = ???
      def addressFromPublicKey(publicKey: ByteStr): Either[String, Address]               = ???
      def accountScript(addressOrAlias: Recipient): Option[Script]                        = ???
      def calculateDelay(gt: ByteStr, b: Long): Long                                      = ???
      def callScript(
          dApp: Address,
          func: String,
          args: List[EVALUATED],
          payments: Seq[(Option[Array[Byte]], Long)],
          remainingComplexity: Int,
          reentrant: Boolean
      ): Coeval[(Either[ValidationError, (EVALUATED, Log[Id])], Int)] = ???
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

        if (
          version == EnvironmentFunctions.AddressVersion && network == chainId && addressBytes.length == EnvironmentFunctions.AddressLength && checksumCorrect
        )
          Right(Some(addressBytes))
        else Right(None)
    }
  }

  @tailrec def fold(totalSize: Int, list: String, acc: String, f: String)(size: Int = totalSize): String =
    if (size == 0)
      acc
    else
      fold(totalSize, list, s"$f($acc, $list[${totalSize - size}])", f)(size - 1)
}

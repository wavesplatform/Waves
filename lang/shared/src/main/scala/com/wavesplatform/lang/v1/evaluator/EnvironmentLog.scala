package com.wavesplatform.lang.v1.evaluator

import cats.Id
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import monix.eval.Coeval

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/** What an evaluation read from its environment, in order, so that a second evaluation of the same script sees the same inputs although
  * the first one's invokes have changed the state since. See docs/notes/ride-evaluator.md.
  */
final class EnvironmentLog {
  private[evaluator] val calls = ArrayBuffer.empty[EnvironmentLog.Call]
}

object EnvironmentLog {
  // A call is identified by its method and arguments; byte arrays compare by content.
  private[evaluator] final case class Call(method: String, args: Seq[Any], result: Any)

  private[evaluator] def sameArgs(a: Seq[Any], b: Seq[Any]): Boolean =
    a.length == b.length && a.lazyZip(b).forall {
      case (x: Array[Byte], y: Array[Byte]) => java.util.Arrays.equals(x, y)
      case (Some(x: Array[Byte]), Some(y: Array[Byte])) => java.util.Arrays.equals(x, y)
      case (x, y) => x == y
    }
}

/** Passes every call on to `underlying` and records its result in `log`. */
final class RecordingEnvironment(underlying: Environment[Id], log: EnvironmentLog) extends Environment[Id] {
  private def record[A](method: String, args: Any*)(result: A): A = {
    log.calls += EnvironmentLog.Call(method, args, result)
    result
  }

  override def chainId: Byte                         = underlying.chainId
  override def inputEntity: Environment.InputEntity  = underlying.inputEntity
  override def tthis: Environment.Tthis              = underlying.tthis
  override def multiPaymentAllowed: Boolean          = underlying.multiPaymentAllowed
  override def txId: ByteStr                         = underlying.txId
  override def dAppAlias: Boolean                    = underlying.dAppAlias
  override def height: Long                          = record("height")(underlying.height)
  override def transactionById(id: Array[Byte]): Option[Tx] = record("transactionById", id)(underlying.transactionById(id))
  override def transferTransactionById(id: Array[Byte]): Option[Tx.Transfer] =
    record("transferTransactionById", id)(underlying.transferTransactionById(id))
  override def transactionHeightById(id: Array[Byte]): Option[Long] = record("transactionHeightById", id)(underlying.transactionHeightById(id))
  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] = record("assetInfoById", id)(underlying.assetInfoById(id))
  override def lastBlockOpt(): Option[BlockInfo]                        = record("lastBlockOpt")(underlying.lastBlockOpt())
  override def blockInfoByHeight(height: Int): Option[BlockInfo] = record("blockInfoByHeight", height)(underlying.blockInfoByHeight(height))
  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] =
    record("data", addressOrAlias, key, dataType)(underlying.data(addressOrAlias, key, dataType))
  override def hasData(addressOrAlias: Recipient): Boolean = record("hasData", addressOrAlias)(underlying.hasData(addressOrAlias))
  override def resolveAlias(name: String): Either[String, Address] = record("resolveAlias", name)(underlying.resolveAlias(name))
  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] =
    record("accountBalanceOf", addressOrAlias, assetId)(underlying.accountBalanceOf(addressOrAlias, assetId))
  override def accountWavesBalanceOf(addressOrAlias: Recipient): Either[String, Environment.BalanceDetails] =
    record("accountWavesBalanceOf", addressOrAlias)(underlying.accountWavesBalanceOf(addressOrAlias))
  override def transferTransactionFromProto(b: Array[Byte]): Option[Tx.Transfer] =
    record("transferTransactionFromProto", b)(underlying.transferTransactionFromProto(b))
  override def addressFromString(address: String): Either[String, Address] =
    record("addressFromString", address)(underlying.addressFromString(address))
  override def addressFromPublicKey(publicKey: ByteStr): Either[String, Address] =
    record("addressFromPublicKey", publicKey)(underlying.addressFromPublicKey(publicKey))
  override def accountScript(addressOrAlias: Recipient): Option[Script] = record("accountScript", addressOrAlias)(underlying.accountScript(addressOrAlias))
  override def calculateDelay(generator: ByteStr, balance: Long): Long =
    record("calculateDelay", generator, balance)(underlying.calculateDelay(generator, balance))
  override def callScript(
      dApp: Address,
      func: String,
      args: List[EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[(Either[ValidationError, (EVALUATED, Log[Id])], Int)] = {
    val callArgs = Seq(dApp, func, args, payments.map { case (asset, amount) => (asset.map(ByteStr(_)), amount) }, availableComplexity, reentrant)
    // The slot is taken now, so that the order of calls is the order they were made in, not the order their results arrived in.
    val index = log.calls.length
    log.calls += EnvironmentLog.Call("callScript", callArgs, null)
    underlying.callScript(dApp, func, args, payments, availableComplexity, reentrant).materialize.map { result =>
      log.calls(index) = EnvironmentLog.Call("callScript", callArgs, result)
      result
    }.dematerialize
  }
}

/** Answers the calls recorded in `log` from the record, in order. Once the record is used up it passes calls on to `underlying`, or throws
  * when `strict`. A call that differs from the recorded one throws: the evaluations no longer agree.
  */
final class ReplayingEnvironment(underlying: Environment[Id], log: EnvironmentLog, strict: Boolean) extends Environment[Id] {
  private var next = 0

  // The recorded calls not replayed.
  def unreplayed: Int = log.calls.length - next

  private def replay[A](method: String, args: Any*)(live: => A): A =
    if (next >= log.calls.length) {
      if (strict) throw new IllegalStateException(s"Environment call $method(${args.mkString(", ")}) made beyond the recorded ones")
      live
    } else {
      val call = log.calls(next)
      if (call.method != method || !EnvironmentLog.sameArgs(call.args, args))
        throw new IllegalStateException(s"Environment call $method(${args.mkString(", ")}) replayed where ${call.method}(${call.args.mkString(", ")}) was made")
      next += 1
      call.result.asInstanceOf[A]
    }

  override def chainId: Byte                         = underlying.chainId
  override def inputEntity: Environment.InputEntity  = underlying.inputEntity
  override def tthis: Environment.Tthis              = underlying.tthis
  override def multiPaymentAllowed: Boolean          = underlying.multiPaymentAllowed
  override def txId: ByteStr                         = underlying.txId
  override def dAppAlias: Boolean                    = underlying.dAppAlias
  override def height: Long                          = replay("height")(underlying.height)
  override def transactionById(id: Array[Byte]): Option[Tx] = replay("transactionById", id)(underlying.transactionById(id))
  override def transferTransactionById(id: Array[Byte]): Option[Tx.Transfer] =
    replay("transferTransactionById", id)(underlying.transferTransactionById(id))
  override def transactionHeightById(id: Array[Byte]): Option[Long] = replay("transactionHeightById", id)(underlying.transactionHeightById(id))
  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] = replay("assetInfoById", id)(underlying.assetInfoById(id))
  override def lastBlockOpt(): Option[BlockInfo]                        = replay("lastBlockOpt")(underlying.lastBlockOpt())
  override def blockInfoByHeight(height: Int): Option[BlockInfo] = replay("blockInfoByHeight", height)(underlying.blockInfoByHeight(height))
  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] =
    replay("data", addressOrAlias, key, dataType)(underlying.data(addressOrAlias, key, dataType))
  override def hasData(addressOrAlias: Recipient): Boolean = replay("hasData", addressOrAlias)(underlying.hasData(addressOrAlias))
  override def resolveAlias(name: String): Either[String, Address] = replay("resolveAlias", name)(underlying.resolveAlias(name))
  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] =
    replay("accountBalanceOf", addressOrAlias, assetId)(underlying.accountBalanceOf(addressOrAlias, assetId))
  override def accountWavesBalanceOf(addressOrAlias: Recipient): Either[String, Environment.BalanceDetails] =
    replay("accountWavesBalanceOf", addressOrAlias)(underlying.accountWavesBalanceOf(addressOrAlias))
  override def transferTransactionFromProto(b: Array[Byte]): Option[Tx.Transfer] =
    replay("transferTransactionFromProto", b)(underlying.transferTransactionFromProto(b))
  override def addressFromString(address: String): Either[String, Address] =
    replay("addressFromString", address)(underlying.addressFromString(address))
  override def addressFromPublicKey(publicKey: ByteStr): Either[String, Address] =
    replay("addressFromPublicKey", publicKey)(underlying.addressFromPublicKey(publicKey))
  override def accountScript(addressOrAlias: Recipient): Option[Script] = replay("accountScript", addressOrAlias)(underlying.accountScript(addressOrAlias))
  override def calculateDelay(generator: ByteStr, balance: Long): Long =
    replay("calculateDelay", generator, balance)(underlying.calculateDelay(generator, balance))
  override def callScript(
      dApp: Address,
      func: String,
      args: List[EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[(Either[ValidationError, (EVALUATED, Log[Id])], Int)] = {
    val callArgs = Seq(dApp, func, args, payments.map { case (asset, amount) => (asset.map(ByteStr(_)), amount) }, availableComplexity, reentrant)
    if (next >= log.calls.length && !strict) underlying.callScript(dApp, func, args, payments, availableComplexity, reentrant)
    else
      Coeval.fromTry(replay("callScript", callArgs*)(null: Try[(Either[ValidationError, (EVALUATED, Log[Id])], Int)]))
  }
}

package com.wavesplatform.ride.runner.environments

import cats.Id
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.traits.Environment.{InputEntity, Tthis}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.transaction.smart.{DAppEnvironment, DAppEnvironmentInterface}
import monix.eval.Coeval

// TrackedDAppEnvironment is better than TrackedBlockchain, because callScript calls to Blockchain not only from the
// script, but from a validation part too. We should re-run a script only if we sure the result will change.
class TrackedDAppEnvironment(underlying: DAppEnvironment, tracker: DAppEnvironmentTracker) extends DAppEnvironmentInterface {
  override def ds: DirectiveSet = underlying.ds

  override def remainingCalls: Int = underlying.remainingCalls

  override def availableActions: DAppEnvironment.ActionLimits = underlying.availableActions

  override def availablePayments: Int = underlying.availablePayments

  override def currentSnapshot: StateSnapshot = underlying.currentSnapshot

  override def invocationRoot: DAppEnvironment.InvocationTreeTracker = underlying.invocationRoot

  override def calculateDelay(generator: ByteStr, balance: Long): Long =
    underlying.calculateDelay(generator, balance)

  // Functions those need Blockchain
  override def height: Id[Long] = {
    tracker.height()
    underlying.height
  }

  override def transactionById(id: Array[Byte]): Id[Option[Tx]] = {
    tracker.transactionById(id)
    underlying.transactionById(id)
  }

  override def transferTransactionById(id: Array[Byte]): Id[Option[Tx.Transfer]] = {
    tracker.transferTransactionById(id)
    underlying.transferTransactionById(id)
  }

  override def transactionHeightById(id: Array[Byte]): Id[Option[Long]] = {
    tracker.transactionHeightById(id)
    underlying.transactionHeightById(id)
  }

  override def assetInfoById(id: Array[Byte]): Id[Option[ScriptAssetInfo]] = {
    tracker.assetInfoById(id)
    underlying.assetInfoById(id)
  }

  override def lastBlockOpt(): Id[Option[BlockInfo]] = {
    tracker.lastBlockOpt()
    underlying.lastBlockOpt()
  }

  override def blockInfoByHeight(height: Int): Id[Option[BlockInfo]] = {
    tracker.blockInfoByHeight(height)
    underlying.blockInfoByHeight(height)
  }

  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Id[Option[Any]] = {
    withResolvedAlias(addressOrAlias).foreach(tracker.data(_, key))
    underlying.data(addressOrAlias, key, dataType)
  }

  override def hasData(addressOrAlias: Recipient): Id[Boolean] = {
    withResolvedAlias(addressOrAlias).foreach(tracker.hasData)
    underlying.hasData(addressOrAlias)
  }

  override def resolveAlias(name: String): Id[Either[String, Recipient.Address]] = {
    tracker.resolveAlias(name)
    underlying.resolveAlias(name)
  }

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Id[Either[String, Long]] = {
    withResolvedAlias(addressOrAlias).foreach(tracker.accountBalanceOf(_, assetId))
    underlying.accountBalanceOf(addressOrAlias, assetId)
  }

  override def accountWavesBalanceOf(addressOrAlias: Recipient): Id[Either[String, Environment.BalanceDetails]] = {
    withResolvedAlias(addressOrAlias).foreach(tracker.accountWavesBalanceOf)
    underlying.accountWavesBalanceOf(addressOrAlias)
  }

  override def accountScript(addressOrAlias: Recipient): Id[Option[Script]] = {
    withResolvedAlias(addressOrAlias).foreach(tracker.accountScript)
    underlying.accountScript(addressOrAlias)
  }

  override def callScript(
      dApp: Recipient.Address,
      func: String,
      args: List[Terms.EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[(Either[ValidationError, (Terms.EVALUATED, Log[Id])], Int)] = {
    toWavesAddress(dApp).foreach(tracker.callScript)
    underlying.callScript(dApp, func, args, payments, availableComplexity, reentrant)
  }

  // Constants
  override def chainId: Byte                = underlying.chainId
  override def inputEntity: InputEntity     = underlying.inputEntity
  override def tthis: Tthis                 = underlying.tthis
  override def multiPaymentAllowed: Boolean = underlying.multiPaymentAllowed
  override def txId: ByteStr                = underlying.txId

  // Utilities
  private def withResolvedAlias(addressOrAlias: Recipient): Option[Address] = addressOrAlias match {
    case addressOrAlias: Recipient.Address => toWavesAddress(addressOrAlias)
    case Recipient.Alias(name)             => resolveAlias(name).flatMap(x => Address.fromBytes(x.bytes.arr, Some(chainId))).toOption
  }

  private def toWavesAddress(addr: Recipient.Address): Option[Address] =
    com.wavesplatform.account.Address.fromBytes(addr.bytes.arr, Some(chainId)).toOption

  // Functions those don't need Blockchain
  override def transferTransactionFromProto(b: Array[Byte]): Id[Option[Tx.Transfer]]       = underlying.transferTransactionById(b)
  override def addressFromString(address: String): Either[String, Recipient.Address]       = underlying.addressFromString(address)
  override def addressFromPublicKey(publicKey: ByteStr): Either[String, Recipient.Address] = underlying.addressFromPublicKey(publicKey)
}

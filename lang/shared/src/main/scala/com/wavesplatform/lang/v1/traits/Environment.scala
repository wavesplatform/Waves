package com.wavesplatform.lang.v1.traits

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.*
import monix.eval.Coeval
import shapeless.*

object Environment {
  case class BalanceDetails(available: Long, regular: Long, generating: Long, effective: Long)

  type InputEntity = Tx :+: Ord :+: PseudoTx :+: CNil

  case class AssetId(id: Array[Byte])
  type Tthis = Recipient.Address :+: AssetId :+: CNil
}

trait Environment[F[_]] {
  def chainId: Byte
  def inputEntity: Environment.InputEntity
  def tthis: Environment.Tthis
  def height: F[Long]
  def transactionById(id: Array[Byte]): F[Option[Tx]]
  def transferTransactionById(id: Array[Byte]): F[Option[Tx.Transfer]]
  def transactionHeightById(id: Array[Byte]): F[Option[Long]]
  def assetInfoById(id: Array[Byte]): F[Option[ScriptAssetInfo]]
  def lastBlockOpt(): F[Option[BlockInfo]]
  def blockInfoByHeight(height: Int): F[Option[BlockInfo]]
  def data(addressOrAlias: Recipient, key: String, dataType: DataType): F[Option[Any]]
  def hasData(addressOrAlias: Recipient): F[Boolean]
  def resolveAlias(name: String): F[Either[String, Recipient.Address]]
  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): F[Either[String, Long]]
  def accountWavesBalanceOf(addressOrAlias: Recipient): F[Either[String, Environment.BalanceDetails]]
  def multiPaymentAllowed: Boolean
  def txId: ByteStr
  def transferTransactionFromProto(b: Array[Byte]): F[Option[Tx.Transfer]]
  def addressFromString(address: String): Either[String, Address]
  def addressFromPublicKey(publicKey: ByteStr): Either[String, Address]
  def dAppAlias: Boolean = false
  def accountScript(addressOrAlias: Recipient): F[Option[Script]]
  def callScript(
      dApp: Address,
      func: String,
      args: List[EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[F[(Either[ValidationError, (EVALUATED, Log[F])], Int)]]
  def calculateDelay(generator: ByteStr, balance: Long): Long
}

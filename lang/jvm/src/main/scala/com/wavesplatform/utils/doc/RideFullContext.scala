package com.wavesplatform.utils.doc

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}

import cats.implicits._

object RideFullContext {
  private val dummyEnv = new Environment {
    override def height: Long = ???
    override def chainId: Byte = 66
    override def inputEntity: InputEntity = ???
    override def transactionById(id: Array[Byte]): Option[Tx] = ???
    override def transferTransactionById(id: Array[Byte]): Option[Tx] = ???
    override def transactionHeightById(id: Array[Byte]): Option[Long] = ???
    override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] = ???
    override def lastBlockOpt(): Option[BlockInfo] = ???
    override def blockInfoByHeight(height: Int): Option[BlockInfo] = ???
    override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
    override def resolveAlias(name: String): Either[String, Recipient.Address] = ???
    override def tthis: Recipient.Address = ???
  }

  def build(
    ds:       DirectiveSet,
    wavesEnv: Environment = dummyEnv
  ): CTX = {
    val wavesCtx = WavesContext.build(ds, wavesEnv)
    val cryptoCtx = CryptoContext.build(Global, ds.stdLibVersion)
    val pureCtx = PureContext.build(Global, ds.stdLibVersion)
    pureCtx |+| cryptoCtx |+| wavesCtx
  }
}

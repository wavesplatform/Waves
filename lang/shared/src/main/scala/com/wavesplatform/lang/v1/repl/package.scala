package com.wavesplatform.lang.v1

import cats.Id
import com.wavesplatform.lang.directives.DirectiveSet.contractDirectiveSet
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import cats.implicits._
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.repl.http.{NodeConnectionSettings, WebEnvironment}

import scala.concurrent.Future

package object repl {
  val global: BaseGlobal = com.wavesplatform.lang.Global
  val errorMsgEnvironment: Environment[Future] = new Environment[Future] {
    lazy val unavailable = throw new RuntimeException(s"Blockchain state is unavailable from REPL")
    override def height: Future[Long]                                                                                    = Future.successful(unavailable)
    override def chainId: Byte                                                                                           = 0
    override def inputEntity: InputEntity                                                                                = unavailable
    override def tthis: Recipient.Address                                                                                = unavailable
    override def transactionById(id: Array[Byte]): Future[Option[Tx]]                                                    = unavailable
    override def transferTransactionById(id: Array[Byte]): Future[Option[Tx]]                                            = unavailable
    override def transactionHeightById(id: Array[Byte]): Future[Option[Long]]                                            = unavailable
    override def assetInfoById(d: Array[Byte]): Future[Option[ScriptAssetInfo]]                                          = unavailable
    override def lastBlockOpt(): Future[Option[BlockInfo]]                                                               = unavailable
    override def blockInfoByHeight(height: Int): Future[Option[BlockInfo]]                                               = unavailable
    override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Future[Option[Any]]                   = unavailable
    override def resolveAlias(name: String): Future[Either[String, Recipient.Address]]                                   = unavailable
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Future[Either[String, Long]] = unavailable
  }

  def buildInitialCtx(settings: Option[NodeConnectionSettings]): CTX[Future] = {
    val environment = settings.fold(errorMsgEnvironment)(WebEnvironment)
    CryptoContext.build(global, V3) |+|
    PureContext.build(global, V3)   |+|
    WavesContext.build(contractDirectiveSet, environment)
  }

  val internalVarPrefixes: Set[Char] = Set('@', '$')
  val internalFuncPrefix: String = "_"
}

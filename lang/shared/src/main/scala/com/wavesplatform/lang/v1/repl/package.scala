package com.wavesplatform.lang.v1

import cats.arrow.FunctionK
import cats.implicits._
import cats.{Id, Monad, ~>}
import com.wavesplatform.lang.directives.DirectiveSet.contractDirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.repl.http.{NodeConnectionSettings, WebEnvironment}
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}

import scala.concurrent.ExecutionContext.Implicits.{global => g}
import scala.concurrent.Future
import scala.util.Try

package object repl {
  val global: BaseGlobal = com.wavesplatform.lang.Global
  val errorMsgEnvironment: Environment[Future] = new Environment[Future] {
    lazy val unavailable = throw new RuntimeException(s"Blockchain state is unavailable from REPL")
    override def height: Future[Long]                                                                                    = Future.successful(0)
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

  implicit val futureMonad: Monad[Future] = Monad[Future]
  val id2Future: Id ~> Future = new FunctionK[Id, Future] {
    def apply[A](a: A): Future[A] = Future.successful(a)
  }

  def buildInitialCtx(settings: Option[NodeConnectionSettings]): CTX[Future] = {
    val environment = settings.fold(errorMsgEnvironment)(WebEnvironment)
    CryptoContext.build(global, V3).mapK(id2Future) |+|
    PureContext.build(global, V3).mapK(id2Future)   |+|
    WavesContext.build[Future](contractDirectiveSet, environment)
  }

  val internalVarPrefixes: Set[Char] = Set('@', '$')
  val internalFuncPrefix: String = "_"

  def method(v: { def f(a: Int): String }): String = v.f(1)



  object O {
    def f(a: Int): String = ""
  }
  class C {
    def f(a: Int): String = ""
  }
  method(O)
  method(new C())
  method(new { def f(a: Int): String = "" })
}

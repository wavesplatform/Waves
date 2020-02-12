package com.wavesplatform.it

import com.softwaremill.sttp._
import NodeRestApi._
import NodesRestApi._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Random

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class NodesRestApi(private[this] val nodes: Seq[Node]) extends  {

  val activation = new ActivationApi(nodes, Seq("activation"))
  val addresses = new AddressesApi(nodes, Seq("addresses"))
  val alias = new AliasApi(nodes, Seq("alias"))
  val assets = new AssetsApi(nodes, Seq("addresses"))
  val blockchain = new BlockchainApi(nodes, Seq("blockchain"))
  val blocks = new BlocksApi(nodes, Seq("blocks"))
  val consensus = new ConsensusApi(nodes, Seq("consensus"))
  val debug = new DebugApi(nodes, Seq("debug"))
  val leasing = new LeasingApi(nodes, Seq("leasing"))
  val peers = new PeersApi(nodes, Seq("peers"))
  val transactions = new TransactionsApi(nodes, Seq("transactions"))
  val utils = new UtilsApi(nodes, Seq("utils"))

  def status = ???
  def stop = ???
  def version = ??? //get("node" :: "version" :: Nil)

  def post(
            path: Seq[String] = List.empty,
            body: String = "",
            apiKey: Option[String] = None,
            retry: Boolean = true
          ): Response[String] = {
    val tasks = nodes.map { n =>
      Future {
        n.api.post(path, body, apiKey, retry)
      }
    }
    val responses = Await.result(Future.sequence(tasks), 10.seconds)
    responses.head //TODO compare responses
  }

  def get(
           path: Seq[String] = List.empty,
           params: Map[String, String] = Map.empty,
           apiKey: Option[String] = None,
           retry: Boolean = true
         ): Response[String] = {
    val tasks = nodes.map { n =>
      Future {
        n.api.get(path, params, apiKey, retry)
      }
    }
    val responses = Await.result(Future.sequence(tasks), 10.seconds)
    responses.head //TODO compare responses
  }

  def forEach[T](task: Node => T) = { //TODO как лучше назвать?
    val tasks = nodes.map(n => Future(task(n)))
    val responses = Await.result(Future.sequence(tasks), 10.seconds)
    responses.head //TODO compare results
  }

  def onRandom[T](task: Node => T) = { //TODO как лучше назвать?
    Await.result(Future(task(nodes(Random.nextInt(nodes.size)))), 10.seconds)
  }

}

object NodesRestApi {

  //noinspection ScalaStyle
  implicit class RestApi(private val nodes: Seq[Node]) {
    val api = new NodesRestApi(nodes)
  }

}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class ActivationApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def status = nodes.api.forEach(node => node.api.activation.status)
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class AddressesApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def get = ??? //TODO onRandom or Seq w/o retry
  def post = ??? //TODO onRandom or Seq w/o retry
  def delete(address: String) = ??? //TODO onRandom or Seq w/o retry
  def balance(address: String) = nodes.api.forEach(node => node.api.addresses.balance(address))
  def balance(address: String, confirmations: Int) = ???
  def balanceDetails(address: String) = ???
  //TODO call get and post via single method and compare inside
  def dataGet(address: String, matches: Option[String] = None, key: Option[Seq[String]] = None) = ???
  //TODO matches?
  def dataPost(address: String, keys: Option[Seq[String]] = None, viaForm: Boolean = false) = ???
  def data(address: String, key: String) = ???
  def effectiveBalance(address: String) = ???
  def effectiveBalance(address: String, confirmations: Int) = ???
  def publicKey(publicKey: String) = ???
  def scriptInfo(address: String) = ???
  def scriptInfoMeta(address: String) = ???
  def seed(address: String) = ??? //TODO onRandom or Seq w/o retry
  def seq(fromAddress: String, toAddress: String) = ???
  def sign(address: String, message: String) = ??? //TODO onRandom or Seq w/o retry
  def signText(address: String, message: String) = ??? //TODO onRandom or Seq w/o retry
  def validate(address: String) = ???
  def verify(address: String, message: String, publicKey: String, signature: String) = ???
  def verifyText(address: String, message: String, publicKey: String, signature: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class AliasApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def byAddress(address: String) = ???
  def byAlias(alias: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class AssetsApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  @Deprecated
  def distribution(assetId: String) = ???
  def distribution(assetId: String, height: Int, limit: Int, after: Option[String] = None) = ???
  def balance(address: String, assetId: Option[String] = None) = ???
  def details(assetId: String, full: Option[Boolean] = None) = ???
  def detailsGet(assetIds: Seq[String], full: Option[Boolean] = None) = ???
  def detailsPost(assetIds: Seq[String], full: Option[Boolean] = None) = ???
  def nft(address: String, limit: Int, after: Option[String] = None) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class BlockchainApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def rewards() = ???
  def rewards(height: Int) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class BlocksApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def address(address: String, from: Int, to: Int) = ???
  def at(height: Int) = ???
  def delay(blockSignature: String, blocksCount: Int) = ???
  def first = ???
  def headersAt(height: Int) = ???
  def headersLast = ??? //TODO onRandom or Seq w/o retry
  def headersSeq(from: Int, to: Int) = ???
  def height = ??? //TODO onRandom or Seq w/o retry
  def height(blockSignature: String) = ???
  def last = ??? //TODO onRandom or Seq w/o retry
  def seq(from: Int, to: Int) = ???
  def signature(signature: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class ConsensusApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def algo = ???
  def baseTarget = ???
  def baseTarget(blockId: String) = ???
  def generatingBalance(address: String) = ???
  def generationSignature = ???
  def generationSignature(blockId: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class DebugApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def balancesHistory(address: String) = ???
  def blacklist(ip: String) = ???
  def blocks(howMany: Int) = ???
  def configInfo(full: Option[Boolean] = None) = ??? //TODO onRandom or Seq w/o retry
  def historyInfo = ??? //TODO onRandom or Seq w/o retry
  def info = ??? //TODO onRandom or Seq w/o retry
  def minerInfo = ??? //TODO onRandom or Seq w/o retry
  def portfolios(address: String, considerUnspent: Option[Boolean] = None) = ???
  def print(message: String) = ???
  def rollback(to: Int, returnTxsToUTX: Boolean) = ???
  def rollbackTo(signature: String) = ???
  def state = ???
  def stateChanges(address: String, limit: Int, after: Option[String] = None) = ???
  def stateChangesInfo(id: String) = ???
  def stateWaves(height: Int) = ???
  def validate(transactionJson: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class LeasingApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def active(address: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class PeersApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def all = ??? //TODO onRandom or Seq w/o retry
  def blacklisted = ??? //TODO onRandom or Seq w/o retry
  def clearBlacklist = ???
  def connect(host: String, port: Int) = ??? //TODO onRandom or Seq w/o retry
  def connected = ??? //TODO onRandom or Seq w/o retry
  def suspended = ??? //TODO onRandom or Seq w/o retry
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class TransactionsApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def address(address: String, limit: Int, after: Option[String] = None) = ???
  def broadcast(txJson: String) = ??? //TODO onRandom + forEach if success
  def calculateFee(txJson: String) = ???
  def info(id: String) = ???
  def merkleProofGet(id: String*) = ???
  def merkleProofPost(id: String*) = ??? //TODO call get and post via single method and compare inside
  def sign(txJson: String) = ??? //TODO onRandom or Seq w/o retry
  def sign(address: String, txJson: String) = ??? //TODO onRandom or Seq w/o retry
  def statusGet(id: String*) = ??? //TODO onRandom or Seq w/o retry
  def statusPost(id: String*) = ??? //TODO call get and post via single method and compare inside
  def unconfirmed = ??? //TODO onRandom or Seq w/o retry
  def unconfirmed(id: String) = ??? //TODO onRandom or Seq w/o retry
  def unconfirmedSize = ??? //TODO onRandom or Seq w/o retry
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class UtilsApi(private val nodes: Seq[Node], private val apiGroup: Seq[String]) {
  def hashFast(message: String) = ???
  def hashSecure(message: String) = ???
  @Deprecated
  def compile(code: String) = ???
  def compileCode(code: String) = ???
  def compileWithImports(script: String, imports: Option[Map[String, String]] = None) = ???
  def decompile(compiledBase64: String) = ???
  def estimate(compiledBase64: String) = ???
  def seed = ??? //TODO onRandom or Seq w/o retry
  def seed(length: Int) = ??? //TODO onRandom or Seq w/o retry
  def sign(privateKey: String, message: String) = ???
  def time = ??? //TODO onRandom or Seq w/o retry
  def transactionSerialize = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class WalletApi(private val nodes: Seq[Node], private val apiGroup: List[String]) {
  def seed = ??? //TODO onRandom or Seq w/o retry
}

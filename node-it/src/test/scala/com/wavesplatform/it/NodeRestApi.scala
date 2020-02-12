package com.wavesplatform.it

import com.softwaremill.sttp._
import com.wavesplatform.http.`X-Api-Key`

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class NodeRestApi(private[this] val node: Node) extends  {
  private[this] implicit val backend: SttpBackend[Id, Nothing] = NodeRestApi.sttpBackend

  val activation = new ActivationApi(this, Seq("activation"))
  val addresses = new AddressesApi(this, Seq("addresses"))
  val alias = new AliasApi(this, Seq("alias"))
  val assets = new AssetsApi(this, Seq("addresses"))
  val blockchain = new BlockchainApi(this, Seq("blockchain"))
  val blocks = new BlocksApi(this, Seq("blocks"))
  val consensus = new ConsensusApi(this, Seq("consensus"))
  val debug = new DebugApi(this, Seq("debug"))
  val leasing = new LeasingApi(this, Seq("leasing"))
  val peers = new PeersApi(this, Seq("peers"))
  val transactions = new TransactionsApi(this, Seq("transactions"))
  val utils = new UtilsApi(this, Seq("utils"))
  val wallet = new WalletApi(this, Seq("wallet"))

  def status = ???
  def stop = ???
  def version(address: String) = ??? //get("node" :: "version" :: Nil)

  def post(path: Seq[String] = List.empty,
           body: String = "",
           apiKey: Option[String] = None,
           retry: Boolean = true /*TODO use*/): Response[String] = {
    //TODO mode "application/x-www-form-urlencoded"
    val headers = if (apiKey.isDefined) Map(`X-Api-Key`.name -> apiKey.get) else Map[String, String]()
    sttp
      .headers(headers)
      .contentType("application/json", "utf-8")
      .post(uri"${node.nodeApiEndpoint}$path")
      .body(body)
      .send()
  }

  def get(
           path: Seq[String] = List.empty,
           params: Map[String, String] = Map.empty,
           apiKey: Option[String] = None,
           retry: Boolean = true //TODO
         ): Response[String] = {
    val headers = if (apiKey.isDefined) Map(`X-Api-Key`.name -> apiKey.get) else Map[String, String]()
    sttp
      .headers(headers)
      .get(uri"${node.nodeApiEndpoint}$path?$params")
      .send()
  }

}

object NodeRestApi {
  implicit private val sttpBackend: SttpBackend[Id, Nothing] = HttpURLConnectionBackend()

  //noinspection ScalaStyle
  implicit class RestApi(private val node: Node) {
    val api = new NodeRestApi(node)
  }

}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class ActivationApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def status: Response[String] = api.get(apiGroup :+ "status")
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class AddressesApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def get = ???
  def create = ???
  def delete(address: String) = ???
  def balance(address: String): Response[String] = api.get(apiGroup :+ "balance" :+ address)
  def balance(address: String, confirmations: Int) = ???
  def balanceDetails(address: String) = ???
  //TODO call get and post via single method and compare inside
  def dataGet(address: String, matches: Option[String] = None, key: Option[Seq[String]] = None) = ???
  //TODO "matches" param?
  def dataPost(address: String, keys: Option[Seq[String]] = None, viaForm: Boolean = false) = ???
  def data(address: String, key: String) = ???
  def effectiveBalance(address: String) = ???
  def effectiveBalance(address: String, confirmations: Int) = ???
  def publicKey(publicKey: String) = ???
  def scriptInfo(address: String) = ???
  def scriptInfoMeta(address: String) = ???
  def seed(address: String) = ???
  def seq(fromAddress: String, toAddress: String) = ???
  def sign(address: String, message: String) = ???
  def signText(address: String, message: String) = ???
  def validate(address: String) = ???
  def verify(address: String, message: String, publicKey: String, signature: String) = ???
  def verifyText(address: String, message: String, publicKey: String, signature: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class AliasApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def byAddress(address: String) = ???
  def byAlias(alias: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class AssetsApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
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
class BlockchainApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def rewards() = ???
  def rewards(height: Int) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class BlocksApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def address(address: String, from: Int, to: Int) = ???
  def at(height: Int) = ???
  def delay(blockSignature: String, blocksCount: Int) = ???
  def first = ???
  def headersAt(height: Int) = ???
  def headersLast = ???
  def headersSeq(from: Int, to: Int) = ???
  def height = ???
  def height(blockSignature: String) = ???
  def last = ???
  def seq(from: Int, to: Int) = ???
  def signature(signature: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class ConsensusApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def algo = ???
  def baseTarget = ???
  def baseTarget(blockId: String) = ???
  def generatingBalance(address: String) = ???
  def generationSignature = ???
  def generationSignature(blockId: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class DebugApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def balancesHistory(address: String) = ???
  def blacklist(ip: String) = ???
  def blocks(howMany: Int) = ???
  def configInfo(full: Option[Boolean] = None) = ???
  def historyInfo = ???
  def info = ???
  def minerInfo = ???
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
class LeasingApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def active(address: String) = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class PeersApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def all = ???
  def blacklisted = ???
  def clearBlacklist = ???
  def connect(host: String, port: Int) = ???
  def connected = ???
  def suspended = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class TransactionsApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def address(address: String, limit: Int, after: Option[String] = None) = ???
  def broadcast(txJson: String) = ???
  def calculateFee(txJson: String) = ???
  def info(id: String) = ???
  def merkleProofGet(id: String*) = ???
  def merkleProofPost(id: String*) = ??? //TODO call get and post via single method and compare inside
  def sign(txJson: String) = ???
  def sign(address: String, txJson: String) = ???
  def statusGet(id: String*) = ???
  def statusPost(id: String*) = ??? //TODO call get and post via single method and compare inside
  def unconfirmed = ???
  def unconfirmed(id: String) = ???
  def unconfirmedSize = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class UtilsApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def hashFast(message: String) = ???
  def hashSecure(message: String) = ???
  @Deprecated
  def compile(code: String) = ???
  def compileCode(code: String) = ???
  def compileWithImports(script: String, imports: Option[Map[String, String]] = None) = ???
  def decompile(compiledBase64: String) = ???
  def estimate(compiledBase64: String) = ???
  def seed = ???
  def seed(length: Int) = ???
  def sign(privateKey: String, message: String) = ???
  def time = ???
  def transactionSerialize = ???
}

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class WalletApi(private val api: NodeRestApi, private val apiGroup: Seq[String]) {
  def seed = ???
}

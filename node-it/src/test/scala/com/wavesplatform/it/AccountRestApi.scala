package com.wavesplatform.it

import NodesRestApi._

//noinspection TypeAnnotation,ScalaStyle,NotImplementedCode
class AccountRestApi(private[this] val acc: Account) {
  private[this] def api = acc.nodes.api

  def balance = api.addresses.balance(acc.address)
  def balance(assetId: String) = api.assets.balance(acc.address, Some(assetId))
  def assetsBalance = api.assets.balance(acc.address)
  def data = {
    val get = api.addresses.dataGet(acc.address)
    val post = api.addresses.dataPost(acc.address, viaForm = false)
    val postForm = api.addresses.dataPost(acc.address, viaForm = true)
    get //TODO compare all results
  }
  def data(keys: Seq[String]) = {
    val get = api.addresses.dataGet(acc.address, key = Some(keys))
    val post = api.addresses.dataPost(acc.address, keys = Some(keys), viaForm = false)
    val postForm = api.addresses.dataPost(acc.address, keys = Some(keys), viaForm = true)
    get //TODO compare all results
  }
  def data(keys: Seq[String], matches: String) = api.addresses.dataGet(acc.address, Some(matches), Some(keys))
  def data(key: String) = data(Seq(key))
  def scriptInfo = api.addresses.scriptInfo(acc.address)
  def scriptInfoMeta = api.addresses.scriptInfoMeta(acc.address)
  def transactions(limit: Int = 100) = api.transactions.address(acc.address, limit)
  def transactions(limit: Int = 100, after: String) = api.transactions.address(acc.address, limit, Some(after))
  def nft(limit: Int = 100) = api.assets.nft(acc.address, limit)
  def nft(limit: Int = 100, after: String) = api.assets.nft(acc.address, limit, Some(after))

  def issue = ???
  def transfer = ???
  def reissue = ???
  def burn = ???
  def exchange = ???
  def lease = ???
  def leaseCancel = ???
  def alias = ???
  def massTransfer = ???
  def writeData = ???
  def setScript = ???
  def sponsorship = ???
  def setAssetScript = ???
  def invoke = ???
  def updateAssetInfo = ???

  def broadcast(json: String) = api.transactions.broadcast(json) //TODO или Transaction?
}

object AccountRestApi {

  implicit class RestApi(val acc: Account) {
    val api = new AccountRestApi(acc)
  }

}

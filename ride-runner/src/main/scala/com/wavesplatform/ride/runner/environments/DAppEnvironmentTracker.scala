package com.wavesplatform.ride.runner.environments

import com.wavesplatform.account.Address

trait DAppEnvironmentTracker {
  def height(): Unit
  def transactionById(id: Array[Byte]): Unit
  def transferTransactionById(id: Array[Byte]): Unit
  def transactionHeightById(id: Array[Byte]): Unit
  def assetInfoById(id: Array[Byte]): Unit
  def lastBlockOpt(): Unit
  def blockInfoByHeight(height: Int): Unit
  def data(addressOrAlias: Address, key: String): Unit
  def hasData(address: Address): Unit
  def resolveAlias(name: String): Unit
  def accountBalanceOf(address: Address, assetId: Option[Array[Byte]]): Unit
  def accountWavesBalanceOf(address: Address): Unit
  def accountScript(address: Address): Unit
  def callScript(dApp: Address): Unit
}

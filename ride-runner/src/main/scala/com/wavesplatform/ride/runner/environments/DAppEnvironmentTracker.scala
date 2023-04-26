package com.wavesplatform.ride.runner.environments

import com.wavesplatform.lang.v1.traits.domain.Recipient

trait DAppEnvironmentTracker {
  def height(): Unit
  def transactionById(id: Array[Byte]): Unit
  def transferTransactionById(id: Array[Byte]): Unit
  def transactionHeightById(id: Array[Byte]): Unit
  def assetInfoById(id: Array[Byte]): Unit
  def lastBlockOpt(): Unit
  def blockInfoByHeight(height: Int): Unit
  def data(addressOrAlias: Recipient, key: String): Unit
  def hasData(addressOrAlias: Recipient): Unit
  def resolveAlias(name: String): Unit
  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Unit
  def accountWavesBalanceOf(addressOrAlias: Recipient): Unit
  def accountScript(addressOrAlias: Recipient): Unit
  def callScript(dApp: Recipient.Address): Unit
}

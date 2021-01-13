package com.wavesplatform

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSCalculator.{generationSignature, hit}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.FunctionalitySettings

import scala.annotation.tailrec

package object consensus {
  def calculateInitialBaseTarget(
      keyPair: KeyPair,
      balance: Long,
      functionalitySettings: FunctionalitySettings,
      averageBlockDelay: Long
  ): Long = {
    val posCalculator: PoSCalculator =
      if (functionalitySettings.isFeatureActivated(BlockchainFeatures.FairPoS, 1))
        if (functionalitySettings.isFeatureActivated(BlockchainFeatures.BlockV5, 1)) FairPoSCalculator.fromSettings(functionalitySettings)
        else FairPoSCalculator.V1
      else NxtPoSCalculator

    val hitSource = ByteStr(new Array[Byte](crypto.DigestLength))

    def getHit(account: KeyPair): BigInt = {
      val gs = if (functionalitySettings.isFeatureActivated(BlockchainFeatures.BlockV5, 1)) {
        val vrfProof = crypto.signVRF(account.privateKey, hitSource.arr)
        crypto.verifyVRF(vrfProof, hitSource.arr, account.publicKey).map(_.arr).explicitGet()
      } else generationSignature(hitSource, account.publicKey)

      hit(gs)
    }

    @tailrec def calculateBaseTarget(keyPair: KeyPair, minBT: Long, maxBT: Long, balance: Long): Long =
      if (maxBT - minBT <= 1) maxBT
      else {
        val newBT = (maxBT + minBT) / 2
        val delay = posCalculator.calculateDelay(getHit(keyPair), newBT, balance)
        if (math.abs(delay - averageBlockDelay) < 100) newBT
        else {
          val (min, max) = if (delay > averageBlockDelay) (newBT, maxBT) else (minBT, newBT)
          calculateBaseTarget(keyPair, min, max, balance)
        }
      }

    calculateBaseTarget(keyPair, PoSCalculator.MinBaseTarget, 1000000, balance)
  }

}

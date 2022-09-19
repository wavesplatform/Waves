package com.wavesplatform

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.traits.Environment.BalanceDetails
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.transfer.TransferTransaction

package object ride {

  case class EvaluateBlockchain(
      accounts: Map[Address, EvaluateAccount] = Map.empty,
      assets: Map[IssuedAsset, EvaluateAsset] = Map.empty,
      blocks: Map[Int, EvaluateBlock] = Map.empty,
      // Covers: https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#transactionheightbyid
      transactionHeights: Map[ByteStr, Int] = Map.empty,
      // Covers https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#transfertransactionbyid
      // Fits the blockchain
      // TODO: Usable?
      transfers: Map[ByteStr, TransferTransaction] = Map.empty
  )

  // TODO fits blockchain?
  case class EvaluateAccount(
      // Covers all of https://docs.waves.tech/en/ride/functions/built-in-functions/account-data-storage-functions
      // Covers all of https://docs.waves.tech/en/ride/functions/built-in-functions/data-transaction-functions;
      data: Map[String, DataEntry[_]] = Map.empty,
      // Covers: https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#address-from-recipient
      alias: Option[String] = None,
      // Covers: https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#assetbalance
      assets: Map[IssuedAsset, Long] = Map.empty,
      // Covers: https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#scripthash
      script: Option[ByteStr] = None, // Serialized
      // Covers: https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#waves-balance
      // Fits the blockchain?
      wavesBalance: BalanceDetails
  )

  // TODO fits blockchain?
  // Covers: https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#assetinfo
  // https://docs.waves.tech/en/ride/structures/common-structures/asset#constructor
  case class EvaluateAsset(
      quantity: Long = Long.MaxValue, // TODO JS Overflow
      decimals: Byte = 8,
      issuer: Option[Address] = None,
      issuerPublicKey: Option[PublicKey] = None,
      reissuable: Boolean = true,
      script: Option[ByteStr] = None,
      minSponsoredFee: Option[Long] = None,
      name: Option[String] = None,
      description: Option[String] = None
  )

  // TODO fits blockchain?
  // Covers: https://docs.waves.tech/en/ride/functions/built-in-functions/blockchain-functions#blockinfobyheight
  // https://docs.waves.tech/en/ride/structures/common-structures/block-info
  // TODO default value for all structure?
  case class EvaluateBlock(
      timestamp: Long = 1663299644216L,
      height: Int = 3296628, // TODO default value?
      baseTarget: Int = 130,
      generationSignature: ByteStr = ByteStr
        .decodeBase58(
          "2DPRxRETAew4uSuyX8EKgRfwB2UmRhLhR7nwE4S62ti1r5u8eTNpJX7MVqKzMyh4vxSKrU1vaMCGtqisUVcabikFdeFJGk5o1cEKC2Din4NPGfPR6DRFKj3hdsWKVCNNCWJt"
        )
        .get,
      generator: Address = Address.fromString("3P98GM4MUnq2sNDKZqcb5gBqtT1zVngBAUr").explicitGet(),
      generatorPublicKey: PublicKey = PublicKey.fromBase58String("5G58z3bsDDEuYb5b5h3cJmRVjA4Jmr589FTePY41SLr1").explicitGet(),
      vrf: ByteStr = ByteStr.decodeBase58("48kAzHwo24qkYuQjUiD91xNALifTX4qn8RB6JG7q7a4u").get
  )

}

package com.wavesplatform

import java.io.File

import com.google.common.primitives.Ints
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.database.{RDB, RocksDBWriter}
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Proofs, TxDecimals, TxPositiveAmount}
import com.wavesplatform.utils.{NTP, ScorexLogging}

import scala.collection.immutable.VectorMap

object RollbackBenchmark extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val settings      = Application.loadApplicationConfig(Some(new File(args(0))))
    val rdb           = RDB.open(settings.dbSettings)
    val time          = new NTP(settings.ntpServer)
    val rocksDBWriter = new RocksDBWriter(rdb, settings.blockchainSettings, settings.dbSettings)

    val issuer = KeyPair(new Array[Byte](32))

    log.info("Generating addresses")

    val addresses = 1 to 18000 map { i =>
      PBRecipients.toAddress(Ints.toByteArray(i) ++ new Array[Byte](Address.HashLength - 4), AddressScheme.current.chainId).explicitGet()
    }

    log.info("Generating issued assets")

    val assets = 1 to 200 map { i =>
      IssueTransaction(
        1.toByte,
        issuer.publicKey,
        ByteString.copyFromUtf8("asset-" + i),
        ByteString.EMPTY,
        TxPositiveAmount.unsafeFrom(100000e2.toLong),
        TxDecimals.unsafeFrom(2.toByte),
        false,
        None,
        TxPositiveAmount.unsafeFrom(1e8.toLong),
        time.getTimestamp(),
        Proofs(ByteStr(new Array[Byte](64))),
        AddressScheme.current.chainId
      )
    }

    log.info("Building genesis block")
    val genesisBlock = Block
      .buildAndSign(
        1.toByte,
        time.getTimestamp(),
        Block.GenesisReference,
        1000,
        Block.GenesisGenerationSignature,
        GenesisTransaction.create(issuer.publicKey.toAddress, 100000e8.toLong, time.getTimestamp()).explicitGet() +: assets,
        issuer,
        Seq.empty,
        -1,
        None
      )
      .explicitGet()

    val map = assets.map(it => IssuedAsset(it.id()) -> 1L).to(VectorMap)
    val portfolios = for {
      address <- addresses
    } yield address -> Portfolio(assets = map)

    log.info("Appending genesis block")
    rocksDBWriter.append(
      Diff(portfolios = portfolios.toMap),
      0,
      0,
      None,
      genesisBlock.header.generationSignature,
      genesisBlock
    )

    val nextBlock =
      Block
        .buildAndSign(
          2.toByte,
          time.getTimestamp(),
          genesisBlock.id(),
          1000,
          Block.GenesisGenerationSignature,
          Seq.empty,
          issuer,
          Seq.empty,
          -1,
          None
        )
        .explicitGet()
    val nextDiff = Diff(portfolios = addresses.map(_ -> Portfolio(1, assets = VectorMap(IssuedAsset(assets.head.id()) -> 1L))).toMap)

    log.info("Appending next block")
    rocksDBWriter.append(nextDiff, 0, 0, None, ByteStr.empty, nextBlock)

    log.info("Rolling back")
    val start = System.nanoTime()
    rocksDBWriter.rollbackTo(1)
    val end = System.nanoTime()
    log.info(f"Rollback took ${(end - start) * 1e-6}%.3f ms")
    rdb.close()
  }
}

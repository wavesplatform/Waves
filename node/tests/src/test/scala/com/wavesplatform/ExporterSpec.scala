package com.wavesplatform

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block
import com.wavesplatform.database.{DBExt, KeyTag, loadBlock}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.{EthTxGenerator, TxHelpers, TxVersion}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.utils.EthConverters.*

import java.io.ByteArrayOutputStream

class ExporterSpec extends FreeSpec with WithDomain {
  private val ProtoBlocksFrom = 4

  // Legacy blocks first, then protobuf blocks holding legacy-bytes, protobuf and Ethereum transactions, plus an empty block.
  private def withMixedChain(f: Domain => Unit): Unit =
    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(
        BlockchainFeatures.BlockV5           -> ProtoBlocksFrom,
        BlockchainFeatures.SynchronousCalls  -> ProtoBlocksFrom,
        BlockchainFeatures.RideV6            -> ProtoBlocksFrom,
        BlockchainFeatures.BlockReward       -> ProtoBlocksFrom
      ),
      Seq(AddrWithBalance(TxHelpers.defaultAddress), AddrWithBalance(TxHelpers.defaultSigner.toEthWavesAddress))
    ) { d =>
      d.appendBlock(Block.NgBlockVersion, TxHelpers.transfer(version = TxVersion.V1), TxHelpers.transfer(version = TxVersion.V2))
      d.appendBlock(Block.NgBlockVersion, TxHelpers.transfer(version = TxVersion.V2))
      d.appendBlock(
        Block.ProtoBlockVersion,
        TxHelpers.transfer(version = TxVersion.V2),
        TxHelpers.transfer(version = TxVersion.V3),
        EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 1, Waves)
      )
      d.appendBlock(Block.ProtoBlockVersion)
      d.appendBlock(Block.ProtoBlockVersion, TxHelpers.transfer(version = TxVersion.V3))
      d.appendBlock(Block.ProtoBlockVersion)
      f(d)
    }

  private def lengthPrefixed(records: Seq[Array[Byte]]): Array[Byte] =
    records.flatMap(r => Ints.toByteArray(r.length) ++ r).toArray

  private def storedSnapshots(d: Domain, height: Int): Seq[Array[Byte]] = {
    val records = Seq.newBuilder[Array[Byte]]
    val prefix  = KeyTag.NthTransactionStateSnapshotAtHeight.prefixBytes ++ Ints.toByteArray(height)
    d.rdb.db.iterateOver(prefix, Some(d.rdb.txSnapshotHandle.handle))(e => records += e.getValue)
    records.result()
  }

  "exports every persisted block after genesis" - {
    "as length-prefixed block bytes" in withMixedChain { d =>
      val height = d.rocksDBWriter.height
      val out    = new ByteArrayOutputStream()

      Exporter.exportBlocks(d.rdb, height, legacy = true, out, None)

      val blocks = (2 to height).map(h => loadBlock(Height(h), d.rdb).get)
      blocks.map(_.header.version) should contain allOf (Block.NgBlockVersion, Block.ProtoBlockVersion)
      out.toByteArray shouldBe lengthPrefixed(blocks.map(_.bytes()))
    }

    "with each block's stored transaction snapshots" in withMixedChain { d =>
      val height    = d.rocksDBWriter.height
      val snapshots = new ByteArrayOutputStream()

      Exporter.exportBlocks(d.rdb, height, legacy = true, new ByteArrayOutputStream(), Some(snapshots))

      val expected = (2 to height).map(h => lengthPrefixed(storedSnapshots(d, h)))
      snapshots.toByteArray shouldBe lengthPrefixed(expected)
    }
  }
}

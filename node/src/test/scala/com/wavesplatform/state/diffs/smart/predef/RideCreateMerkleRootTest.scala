package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.block.Block.*
import com.wavesplatform.common.merkle.Merkle
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.TxVersion.V3

class RideCreateMerkleRootTest extends PropSpec with WithDomain {
  import DomainPresets.*

  property("createMerkleRoot") {
    Seq(PlainBlockVersion, ProtoBlockVersion)
      .foreach { blockVersion =>
        withDomain(RideV4, AddrWithBalance.enoughBalances(secondSigner)) { d =>
          val dApp = TestCompiler(V4).compileContract(
            """
              | @Callable(i)
              | func merkle(proof: List[ByteVector], id: ByteVector, index: Int) =
              |   [ BinaryEntry("root", createMerkleRoot(proof, id, index)) ]
            """.stripMargin
          )
          d.appendBlock(d.createBlock(blockVersion, Seq(setScript(secondSigner, dApp))))

          val transfers = (1 to 5).map(_ => transfer(version = V3))
          d.appendBlock(d.createBlock(blockVersion, transfers))
          val root = d.lastBlock.header.transactionsRoot

          d.transactionsApi
            .transactionProofs(transfers.map(_.id()).toList)
            .foreach { proof =>
              val digests  = ARR(proof.digests.map(b => CONST_BYTESTR(ByteStr(b)).explicitGet()).toVector, limited = false).explicitGet()
              val id       = CONST_BYTESTR(ByteStr(Merkle.hash(transfers.find(_.id() == proof.id).get.bytes()))).explicitGet()
              val index    = CONST_LONG(proof.transactionIndex)
              val invokeTx = invoke(func = Some("merkle"), args = Seq(digests, id, index))
              d.appendBlock(d.createBlock(blockVersion, Seq(invokeTx)))

              d.liquidSnapshot.scriptResults.head._2.error shouldBe None
              d.blockchain.accountData(secondAddress, "root").get.value shouldBe root
            }
        }
      }
  }
}

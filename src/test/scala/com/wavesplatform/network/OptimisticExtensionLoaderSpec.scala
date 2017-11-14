package com.wavesplatform.network

import com.wavesplatform.state2.ByteStr
import com.wavesplatform.{BlockGen, NoShrink}
import io.netty.channel.embedded.EmbeddedChannel
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class OptimisticExtensionLoaderSpec extends FreeSpec with Matchers with BlockGen with PropertyChecks with NoShrink {
  val signatureGen = bytes64gen.map(ByteStr(_))

  val localSignaturesGen = for {
    n <- Gen.chooseNum(1, 10)
    signatures <- Gen.listOfN(n, signatureGen)
  } yield signatures

  val emptyBlockGen = accountGen.flatMap(signer => versionedBlockGen(Seq.empty, signer, 2))
  def blockWithRefGen(reference: ByteStr) = accountGen.flatMap(signer => versionedBlockGen(reference, Seq.empty, signer, 2))

  val randomExtensionBlocksGen = for {
    n <- Gen.chooseNum(2, 10)
    blocks <- Gen.listOfN(n, emptyBlockGen)
  } yield ExtensionBlocks(blocks)

  def extensionBlocksGen(commonParent: ByteStr) = for {
    n <- Gen.chooseNum(3, 10)
    blocks <- Gen.listOfN(n - 1, emptyBlockGen)
    head <- blockWithRefGen(commonParent)
  } yield ExtensionBlocks(head :: blocks)

  "Discards loaded extension when" - {
    "new one is already requested (and keeps the correct one)" in {
      val droppingOnce = for {
        ls1 <- localSignaturesGen
        p1 <- Gen.oneOf(ls1)
        ext1 <- extensionBlocksGen(p1)
        ext2 <- randomExtensionBlocksGen
        ls2 <- localSignaturesGen
        p2 <- Gen.oneOf(ls2)
        ext3 <- extensionBlocksGen(p2)
      } yield (LoadBlockchainExtension(ls1), ext1, ext2, LoadBlockchainExtension(ls2), ext3)

      forAll(droppingOnce) { case (ls1, ext1, ext2, ls2, ext3) =>
        val ch = new EmbeddedChannel(new OptimisticExtensionLoader)
        ch.writeAndFlush(ls1).sync()
        ch.writeInbound(ext1)
        ch.readInbound[ExtensionBlocks]() shouldEqual ext1

        ch.writeAndFlush(ls2).sync()
        ch.writeInbound(ext2)
        ch.writeInbound(ext3)

        ch.readInbound[ExtensionBlocks]() shouldBe ext3

        ch.releaseInbound() shouldBe false
      }
    }
  }

  "Keeps loaded extension when" - {
    "previous one should've been dropped" in {
      val notDroppingWhenNotNeeded = for {
        ls1 <- localSignaturesGen
        p1 <- Gen.oneOf(ls1)
        ext1 <- extensionBlocksGen(p1)
        ls2 <- localSignaturesGen
        ls3 <- localSignaturesGen
        p2 <- Gen.oneOf(ls3)
        ext2 <- extensionBlocksGen(p2)
      } yield (LoadBlockchainExtension(ls1), ext1, LoadBlockchainExtension(ls2), LoadBlockchainExtension(ls3), ext2)

      forAll(notDroppingWhenNotNeeded) { case (ls1, ext1, ls2, ls3, ext2) =>
        val ch = new EmbeddedChannel(new OptimisticExtensionLoader)
        ch.writeAndFlush(ls1).sync()
        ch.writeInbound(ext1)

        ch.readInbound[ExtensionBlocks]() shouldEqual ext1
        ch.writeAndFlush(ls2).sync()
        ch.writeAndFlush(ls3).sync()
        ch.writeInbound(ext2)

        ch.readInbound[ExtensionBlocks]() shouldEqual ext2

        ch.releaseInbound() shouldBe false
      }
    }

    "local signatures don't fully match signatures in optimistic load request" in {
      val nextRequestWithCommonSignature = for {
        ls1 <- localSignaturesGen
        p1 <- Gen.oneOf(ls1)
        ext1 <- extensionBlocksGen(p1)
        commonSig <- Gen.oneOf(ext1.extension.map(_.uniqueId))
        ls <- localSignaturesGen
        ls2 = commonSig :: ls
        p2 <- Gen.oneOf(ls2)
        ext2 <- extensionBlocksGen(p2)
      } yield (LoadBlockchainExtension(ls1), ext1, LoadBlockchainExtension(ls2), ext2)

      forAll(nextRequestWithCommonSignature) { case (ls1, ext1, ls2, ext2) =>
        val ch = new EmbeddedChannel(new OptimisticExtensionLoader)

        ch.writeAndFlush(ls1)
        ch.writeInbound(ext1)

        ch.readInbound[ExtensionBlocks]() shouldEqual ext1

        ch.writeAndFlush(ls2)
        ch.writeInbound(ext2)

        ch.readInbound[ExtensionBlocks]() shouldEqual ext2
      }
    }
  }

  "Does not stall" - {
    "when blockchain was completely loaded before" in {
      val g = for {
        ls <- localSignaturesGen
        ext1 <- extensionBlocksGen(ls.last)
      } yield (LoadBlockchainExtension(ls), ext1)

      forAll(g) { case (ls, ext) =>
        val ch = new EmbeddedChannel(new OptimisticExtensionLoader)

        ch.writeAndFlush(ls)
        ch.writeInbound(ext)

        val loadedExtension = ch.readInbound[ExtensionBlocks]()

        ch.releaseOutbound()

        ch.writeInbound(ExtensionBlocks(Seq.empty))
        val lbe = LoadBlockchainExtension(loadedExtension.extension.map(_.uniqueId))
        ch.writeAndFlush(lbe)

        ch.readOutbound[LoadBlockchainExtension]() shouldEqual lbe
      }
    }
  }
}

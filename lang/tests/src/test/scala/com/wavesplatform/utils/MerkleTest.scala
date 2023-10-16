package com.wavesplatform.utils

import cats.Id
import cats.implicits.*
import com.google.common.primitives.Ints
import com.wavesplatform.common.merkle.*
import com.wavesplatform.common.merkle.Merkle.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.test.*
import org.scalacheck.{Arbitrary, Gen}

import scala.annotation.tailrec
import scala.util.Random

class MerkleTest extends PropSpec {
  private val EmptyNodeHash = hash(Array[Byte](0))

  private def mkScryptoLevels(data: Seq[Message]): Seq[Level] = {
    if (data.isEmpty) Seq(Seq.empty)
    else {
      @tailrec
      def loop(prevLevel: Seq[Digest], acc: Seq[Level]): Seq[Level] = {
        val level = prevLevel
          .grouped(2)
          .collect {
            case Seq(l, r) => hash(ScryptoMerkleProof.InternalNodePrefix +: (l ++ r))
            case Seq(l) =>
              hash(ScryptoMerkleProof.InternalNodePrefix +: (l ++ EmptyNodeHash))
          }
          .toSeq
        if (level.size == 1) level +: acc else loop(level, level +: acc)
      }
      val bottom = data.map(ld => hash(ScryptoMerkleProof.LeafPrefix +: ld))
      loop(bottom, Seq(bottom))
    }
  }

  private def testData() =
    List
      .fill(10)(Random.nextInt(10000))
      .distinct
      .map(Ints.toByteArray)

  property("TRUE on correct proof") {
    val leaves = testData()
    val tree   = mkScryptoLevels(leaves)

    forAll(Gen.oneOf(leaves.zipWithIndex)) { case (leaf, index) =>
      val proofs = Merkle.mkProofs(index, tree)
      val bytes  = proofBytes(index, proofs)
      eval(scriptSrc(tree.head.head, bytes, leaf)) shouldBe Right(CONST_BOOLEAN(true))
    }
  }

  property("FALSE on incorrect proof") {
    val leaves = testData()
    val tree   = mkScryptoLevels(leaves)

    val twoLeavesGen = Gen.pick(2, leaves.zipWithIndex)

    forAll(twoLeavesGen) { tl =>
      val (_, i1)  = tl(0)
      val (l2, i2) = tl(1)

      eval(scriptSrc(tree.head.head, proofBytes(i2, Merkle.mkProofs(i1, tree)), l2)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  property("FALSE on incorrect root") {
    val leaves = testData()
    val tree1  = mkScryptoLevels(leaves)

    val tree2 = mkScryptoLevels(testData())

    forAll(Gen.oneOf(leaves.zipWithIndex)) { case (leaf, index) =>
      eval(scriptSrc(tree2.head.head, proofBytes(index, Merkle.mkProofs(index, tree1)), leaf)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  property("FALSE on arbitrary proof bytes") {
    val leaves = testData()
    val tree   = mkScryptoLevels(leaves)

    forAll(Gen.oneOf(leaves), Gen.containerOf[Array, Byte](Arbitrary.arbitrary[Byte])) { (leaf, bytes) =>
      eval(scriptSrc(tree.head.head, bytes, leaf)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  property("FALSE on arbitrary root bytes") {
    val leaves = testData()
    val tree   = mkScryptoLevels(leaves)

    forAll(Gen.oneOf(leaves.zipWithIndex), Gen.containerOf[Array, Byte](Arbitrary.arbitrary[Byte])) { case ((leaf, index), bytes) =>
      eval(scriptSrc(bytes, proofBytes(index, Merkle.mkProofs(index, tree)), leaf)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  private def eval(code: String, version: StdLibVersion = V3): Either[String, EVALUATED] = {
    val untyped = Parser.parseExpr(code).get.value
    val ctx     = lazyContexts((DirectiveSet(version, Account, Expression).explicitGet(), true, true, true))()
    val evalCtx = ctx.evaluationContext[Id](Common.emptyBlockchainEnvironment())
    val typed   = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => EvaluatorV2.applyCompleted(evalCtx, v._1, LogExtraInfo(), version, true, true, false)._3.leftMap(_.toString))
  }

  private def scriptSrc(root: Array[Byte], proof: Array[Byte], value: Array[Byte]): String = {
    s"""
       |let rootHash = base64'${Base64.encode(root)}'
       |let leafData = base64'${Base64.encode(value)}'
       |let merkleProof = base64'${Base64.encode(proof)}'
       |
       |checkMerkleProof(rootHash, merkleProof, leafData)
     """.stripMargin
  }

  private def scriptCreateRootSrc(proof: Seq[Array[Byte]], value: Array[Byte], index: Int): String = {
    s"""
       |let leafData = base64'${Base64.encode(value)}'
       |let merkleProof = [base64'${proof.map(Base64.encode).mkString("', base64'")}']
       |
       |createMerkleRoot(merkleProof, leafData, $index)
     """.stripMargin
  }

  property("Create root from proof") {
    val leaves = testData()
    val levels = Merkle.mkLevels(leaves)

    forAll(Gen.oneOf(leaves.zipWithIndex)) { case (leaf, index) =>
      val proofs = mkProofs(index, levels).reverse

      eval(scriptCreateRootSrc(proofs, hash(leaf), index), V4) shouldBe CONST_BYTESTR(ByteStr(levels.head.head))
      eval(scriptCreateRootSrc(proofs, hash(leaf), index + (1 << proofs.length)), V4) should produce("out of range allowed by proof list length")
    }
  }

  private def proofBytes(index: Int, proofs: Seq[Array[Byte]]): Array[Byte] =
    proofs.reverse
      .foldLeft(index -> Array.emptyByteArray) { case ((index, buf), proof) =>
        (index / 2, buf ++ Array(((index + 0) % 2).toByte, proof.length.toByte) ++ proof)
      }
      ._2
}

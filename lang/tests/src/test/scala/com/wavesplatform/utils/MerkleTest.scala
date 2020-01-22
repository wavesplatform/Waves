package com.wavesplatform.utils

import cats.Id
import cats.syntax.monoid._
import com.google.common.primitives.Ints
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import scorex.crypto.authds.merkle.{Leaf, MerkleProof, MerkleTree}
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash.{Blake2b256, CryptographicHash32, Digest, Digest32}

import scala.util.Random

class MerkleTest extends PropSpec with PropertyChecks with Matchers {

  val AMT: Long = 1000000 * 100000000L

  implicit val fastHash = new CryptographicHash32 {
    override def hash(input: Message): Digest32 = Blake2b256.hash(input)
  }

  def testData(): (MerkleTree[Digest32], List[LeafData]) = {
    val data: List[LeafData] =
      List
        .fill(100)(Random.nextInt(10000))
        .distinct
        .map(Ints.toByteArray)
        .map(LeafData @@ _)

    val tree = MerkleTree[Digest32](data)(fastHash)

    (tree, data)
  }

  property("TRUE on correct proof") {
    val (tree, leafs) = testData()

    forAll(Gen.oneOf(leafs)) { leaf =>
      val proof = tree
        .proofByElement(Leaf[Digest32](leaf)(fastHash))
        .get

      eval(scriptSrc(tree.rootHash, proofBytes(proof), leaf)) shouldBe Right(CONST_BOOLEAN(true))
    }
  }

  property("FALSE on incorrect proof") {
    val (tree, leafs) = testData()

    val twoLeafsGen: Gen[(LeafData, LeafData)] =
      for {
        l1 <- Gen.oneOf(leafs)
        l2 <- Gen.oneOf(leafs).suchThat(_ != l1)
      } yield (l1, l2)

    forAll(twoLeafsGen) {
      case (l1, l2) =>
        val proof = tree
          .proofByElement(Leaf[Digest32](l1)(fastHash))
          .get

        eval(scriptSrc(tree.rootHash, proofBytes(proof), l2)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  property("FALSE on incorrect root") {
    val (tree1, leafs) = testData()
    val (tree2, _)     = testData()

    forAll(Gen.oneOf(leafs)) { leaf =>
      val proof = tree1
        .proofByElement(Leaf[Digest32](leaf)(fastHash))
        .get

      eval(scriptSrc(tree2.rootHash, proofBytes(proof), leaf)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  property("FALSE on incorrect proof bytes") {
    val (tree, leafs) = testData()

    forAll(Gen.oneOf(leafs), Gen.containerOf[Array, Byte](Arbitrary.arbitrary[Byte])) { (leaf, bytes) =>
      eval(scriptSrc(tree.rootHash, bytes, leaf)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  property("FALSE on incorrect root bytes") {
    val (tree, leafs) = testData()

    forAll(Gen.oneOf(leafs), Gen.containerOf[Array, Byte](Arbitrary.arbitrary[Byte])) { (leaf, bytes) =>
      val proof = tree
        .proofByElement(Leaf[Digest32](leaf)(fastHash))
        .get

      eval(scriptSrc(bytes, proofBytes(proof), leaf)) shouldBe Right(CONST_BOOLEAN(false))
    }
  }

  private val evaluator = new EvaluatorV1[Id, NoContext]()

  private def eval[T <: EVALUATED](code: String): Either[String, T] = {
    val untyped  = Parser.parseExpr(code).get.value
    val ctx: CTX[NoContext] = PureContext.build(Global, V3) |+| CryptoContext.build(Global, V3)
    val typed    = ExpressionCompiler(ctx.compilerContext, untyped)
    typed.flatMap(v => evaluator.apply[T](ctx.evaluationContext, v._1))
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

  private def proofBytes(mp: MerkleProof[Digest32]): Array[Byte] = {
    def loop(lvls: List[(Digest, Side)], acc: Array[Byte]): Array[Byte] = {
      lvls match {
        case (d, s) :: xs => loop(xs, Array.concat(acc, s +: d.length.toByte +: d))
        case Nil          => acc
      }
    }

    loop(mp.levels.toList, Array.emptyByteArray)
  }
}

package com.wavesplatform.http

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.ApiError.{InvalidAddress, InvalidBase58, InvalidSignature, InvalidTransactionId, TooBigArrayAllocation}
import com.wavesplatform.api.http.TransactionsApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.block.TransactionsRootSpec._
import com.wavesplatform.block.merkle.Merkle.TransactionProof
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{BlockGen, NoShrink, TestTime, TransactionGen}
import monix.eval.Coeval
import monix.reactive.Observable
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

import scala.util.Random

class TransactionsRouteSpec
    extends RouteSpec("/transactions")
    with RestAPISettingsHelper
    with MockFactory
    with Matchers
    with TransactionGen
    with BlockGen
    with PropertyChecks
    with OptionValues
    with NoShrink {

  private val wallet              = Wallet(WalletSettings(None, Some("qwerty"), None))
  private val blockchain          = mock[Blockchain]
  private val utxPoolSynchronizer = mock[UtxPoolSynchronizer]
  private val addressTransactions = mock[CommonTransactionsApi]
  private val utxPoolSize         = mockFunction[Int]

  private val route =
    seal(
      new TransactionsApiRoute(
        restAPISettings,
        addressTransactions,
        wallet,
        blockchain,
        Coeval(utxPoolSize.apply()),
        utxPoolSynchronizer,
        new TestTime
      ).route
    )

  private val invalidBase58Gen = alphaNumStr.map(_ + "0")

  routePath("/calculateFee") - {
    "transfer with Waves fee" - {
      val transferTxScenario =
        for {
          sender    <- accountGen
          recipient <- accountGen
          version   <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
          tx = Json.obj(
            "type"            -> 4,
            "version"         -> version,
            "amount"          -> 1000000,
            "senderPublicKey" -> Base58.encode(sender.publicKey),
            "recipient"       -> recipient.toAddress
          )
        } yield (sender.publicKey, tx)
      "TransferTransaction" in forAll(transferTxScenario) {
        case (sender, transferTx) =>
          (addressTransactions.calculateFee _).expects(*).returning(Right((Asset.Waves, 100000L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
          }
      }

      val massTransferTxScenario =
        for {
          sender     <- accountGen
          recipient1 <- accountGen
          recipient2 <- accountGen
          version    <- Gen.oneOf(MassTransferTransaction.supportedVersions.toSeq)
          tx = Json.obj(
            "type"            -> 11,
            "version"         -> version,
            "senderPublicKey" -> Base58.encode(sender.publicKey),
            "transfers" -> Json.arr(
              Json.obj(
                "recipient" -> recipient1.toAddress,
                "amount"    -> 1000000
              ),
              Json.obj(
                "recipient" -> recipient2.toAddress,
                "amount"    -> 2000000
              )
            )
          )
        } yield (sender.publicKey, tx)
      "MassTransferTransaction" in forAll(massTransferTxScenario) {
        case (sender, transferTx) =>
          (addressTransactions.calculateFee _).expects(*).returning(Right((Asset.Waves, 200000L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 200000
          }
      }
    }

    "transfer with Asset fee" - {
      val transferTxWithAssetFeeScenario =
        for {
          assetId   <- issueGen.map(_.assetId)
          sender    <- accountGen
          recipient <- accountGen
          version   <- Gen.oneOf(TransferTransaction.supportedVersions.toSeq)
          tx = Json.obj(
            "type"            -> 4,
            "version"         -> version,
            "amount"          -> 1000000,
            "feeAssetId"      -> assetId.toString,
            "senderPublicKey" -> Base58.encode(sender.publicKey),
            "recipient"       -> recipient.toAddress
          )
        } yield (sender.publicKey, tx, IssuedAsset(assetId))
      "without sponsorship" in forAll(transferTxWithAssetFeeScenario) {
        case (sender, transferTx, _) =>
          (addressTransactions.calculateFee _).expects(*).returning(Right((Asset.Waves, 100000L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
          }
      }

      "with sponsorship" in {
        val assetId: IssuedAsset = IssuedAsset(issueGen.sample.get.assetId)
        val sender: PublicKey    = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.id.toString,
          "senderPublicKey" -> Base58.encode(sender),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        (addressTransactions.calculateFee _).expects(*).returning(Right((assetId, 5L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe assetId.id.toString
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 5
          }
      }

      "with sponsorship, smart token and smart account" in {
        val assetId: IssuedAsset = IssuedAsset(issueGen.sample.get.assetId)
        val sender: PublicKey    = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.id.toString,
          "senderPublicKey" -> Base58.encode(sender),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        (addressTransactions.calculateFee _).expects(*).returning(Right((assetId, 45L, 0L))).once()

          Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            (responseAs[JsObject] \ "feeAssetId").as[String] shouldBe assetId.id.toString
            (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 45
          }
      }
    }
  }

  routePath("/address/{address}/limit/{limit}") - {
    val bytes32StrGen = bytes32gen.map(Base58.encode)
    val addressGen    = accountGen.map(_.stringRepr)

    "handles parameter errors with corresponding responses" - {
      "invalid address" in {
        forAll(bytes32StrGen) { badAddress =>
          Get(routePath(s"/address/$badAddress/limit/1")) ~> route should produce(InvalidAddress)
        }
      }

      "invalid limit" - {
        "limit is too big" in {
          forAll(addressGen, choose(MaxTransactionsPerRequest + 1, Int.MaxValue).label("limitExceeded")) {
            case (address, limit) =>
              Get(routePath(s"/address/$address/limit/$limit")) ~> route should produce(TooBigArrayAllocation)
          }
        }
      }

      "invalid after" in {
        forAll(addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect"), invalidBase58Gen) {
          case (address, limit, invalidBase58) =>
            Get(routePath(s"/address/$address/limit/$limit?after=$invalidBase58")) ~> route ~> check {
              status shouldEqual StatusCodes.BadRequest
              (responseAs[JsObject] \ "message").as[String] shouldEqual s"Unable to decode transaction id $invalidBase58"
            }
        }
      }
    }

    "returns 200 if correct params provided" - {
      "address and limit" in {
        forAll(addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect")) {
          case (address, limit) =>
            (addressTransactions.aliasesOfAddress _).expects(*).returning(Observable.empty).once
            (addressTransactions.transactionsByAddress _).expects(*, *, *, None).returning(Observable.empty).once()
            Get(routePath(s"/address/$address/limit/$limit")) ~> route ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }

      "address, limit and after" in {
        forAll(addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect"), bytes32StrGen) {
          case (address, limit, txId) =>
            (addressTransactions.aliasesOfAddress _).expects(*).returning(Observable.empty).once
            (addressTransactions.transactionsByAddress _).expects(*, *, *, *).returning(Observable.empty).once()
            Get(routePath(s"/address/$address/limit/$limit?after=$txId")) ~> route ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }
    }
  }

  routePath("/info/{id}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidTransactionId("Wrong char"), matchMsg = true)
      }

      Get(routePath(s"/info/")) ~> route should produce(InvalidTransactionId("Wrong char"), matchMsg = true)
      Get(routePath(s"/info")) ~> route should produce(InvalidTransactionId("Wrong char"), matchMsg = true)
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx     <- randomTransactionGen
        height <- posNum[Int]
      } yield (tx, height)

      forAll(txAvailability) {
        case (tx, height) =>
          (addressTransactions.transactionById _).expects(tx.id()).returning(Some(Height(height) -> Left(tx))).once()

          Get(routePath(s"/info/${tx.id().toString}")) ~> route ~> check {
            status shouldEqual StatusCodes.OK
            responseAs[JsValue] shouldEqual tx.json() + ("height" -> JsNumber(height))
          }
      }
    }
  }

  routePath("/unconfirmed") - {
    "returns the list of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (addressTransactions.unconfirmedTransactions _).expects().returning(txs).once()
        Get(routePath("/unconfirmed")) ~> route ~> check {
          val resp = responseAs[Seq[JsValue]]
          for ((r, t) <- resp.zip(txs)) {
            if ((r \ "version").as[Int] == 1) {
              (r \ "signature").as[String] shouldEqual t.proofs.proofs.head.toString
            } else {
              (r \ "proofs").as[Seq[String]] shouldEqual t.proofs.proofs.map(_.toString)
            }
          }
        }
      }
    }
  }

  routePath("/unconfirmed/size") - {
    "returns the size of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        utxPoolSize.expects().returning(txs.size).once()
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
      }
    }
  }

  routePath("/unconfirmed/info/{id}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/unconfirmed/info/$invalidBase58")) ~> route should produce(InvalidBase58)
      }

      Get(routePath(s"/unconfirmed/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/unconfirmed/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      forAll(randomTransactionGen) { tx =>
        (addressTransactions.unconfirmedTransactionById _).expects(tx.id()).returns(Some(tx)).once()
        Get(routePath(s"/unconfirmed/info/${tx.id().toString}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual tx.json()
        }
      }
    }
  }

  routePath("/sign") - {
    "function call without args" in {
      val acc1 = wallet.generateNewAccount().get
      val acc2 = wallet.generateNewAccount().get

      val funcName          = "func"
      val funcWithoutArgs   = Json.obj("function" -> funcName)
      val funcWithEmptyArgs = Json.obj("function" -> funcName, "args" -> JsArray.empty)
      val funcWithArgs = InvokeScriptTransaction.serializer.functionCallToJson(
        FUNCTION_CALL(
          FunctionHeader.User(funcName),
          List(CONST_LONG(1), CONST_BOOLEAN(true))
        )
      )

      def invoke(func: JsObject, expectedArgsLength: Int): Unit = {
        val ist = Json.obj(
          "type"       -> InvokeScriptTransaction.typeId,
          "version"    -> Gen.oneOf(InvokeScriptTransaction.supportedVersions.toSeq).sample.get,
          "sender"     -> acc1.stringRepr,
          "dApp"       -> acc2.stringRepr,
          "call"       -> func,
          "payment"    -> Seq[Payment](),
          "fee"        -> 500000,
          "feeAssetId" -> JsNull
        )
        Post(routePath("/sign"), ist) ~> ApiKeyHeader ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val jsObject = responseAs[JsObject]
          (jsObject \ "senderPublicKey").as[String] shouldBe acc1.publicKey.toString
          (jsObject \ "call" \ "function").as[String] shouldBe funcName
          (jsObject \ "call" \ "args").as[JsArray].value.length shouldBe expectedArgsLength
        }
      }

      invoke(funcWithoutArgs, 0)
      invoke(funcWithEmptyArgs, 0)
      invoke(funcWithArgs, 2)
    }
  }

  routePath("/merkleProof") - {

    val transactionsGen = for {
      txsSize <- Gen.choose(1, 10)
      txs     <- Gen.listOfN(txsSize, randomTransactionGen)
    } yield txs

    val validBlockGen = for {
      txs    <- transactionsGen
      signer <- accountGen
      block  <- versionedBlockGen(txs, signer, Block.ProtoBlockVersion)
    } yield block

    val invalidBlockGen = for {
      txs     <- transactionsGen
      signer  <- accountGen
      version <- Gen.choose(Block.GenesisBlockVersion, Block.RewardBlockVersion)
      block   <- versionedBlockGen(txs, signer, version)
    } yield block

    val validBlocksGen =
      for {
        blockchainHeight <- Gen.choose(1, 20)
        blocks           <- Gen.listOfN(blockchainHeight, validBlockGen)
      } yield blocks

    val invalidBlocksGen =
      for {
        blockchainHeight <- Gen.choose(1, 10)
        blocks           <- Gen.listOfN(blockchainHeight, invalidBlockGen)
      } yield blocks

    def validateSuccess(txIdsToBlock: Map[String, Block], response: HttpResponse): Unit = {
      response.status shouldBe StatusCodes.OK

      val proofs = responseAs[List[JsObject]]

      proofs.size shouldBe txIdsToBlock.size

      proofs.foreach { p =>
        val transactionId    = (p \ "id").as[String]
        val transactionIndex = (p \ "transactionIndex").as[Int]
        val digests          = (p \ "merkleProof").as[List[String]].map(Base64.decode)

        val block       = txIdsToBlock(transactionId)
        val transaction = block.transactionData.find(_.id().toString == transactionId)

        transaction shouldBe 'defined
        txIdsToBlock.keySet should contain(transactionId)
        transactionIndex shouldBe block.transactionData.indexOf(transaction.value)

        block.verifyTransactionProof(TransactionProof(transaction.value.id(), transactionIndex, digests)) shouldBe true
      }
    }

    def validateFailure(response: HttpResponse): Unit = {
      response.status shouldEqual StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldEqual s"transactions do not exists or block version < ${Block.ProtoBlockVersion}"
    }

    "returns merkle proofs" in {
      forAll(validBlocksGen) { blocks =>
        val txIdsToBlock = blocks.flatMap(b => b.transactionData.map(tx => (tx.id().toString, b))).toMap
        (addressTransactions.transactionProofs _).expects(*).returning(Nil).anyNumberOfTimes()

        val queryParams = txIdsToBlock.keys.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> txIdsToBlock.keySet)

        println(s"\n\t$queryParams\n")

        Get(routePath(s"/merkleProof$queryParams")) ~> route ~> check {
          validateSuccess(txIdsToBlock, response)
        }

        Post(routePath("/merkleProof"), requestBody) ~> route ~> check {
          validateSuccess(txIdsToBlock, response)
        }
      }
    }

    "filters non-existing and 'old'-block transactions" in {
      val gen = validBlocksGen.flatMap(bs => invalidBlocksGen.flatMap(ibs => transactionsGen.map(txs => (bs, ibs, txs))))
      forAll(gen) {
        case (validBlocks, invalidBlocks, unknownTransactions) =>
          val txIdsToBlock = validBlocks.flatMap(b => b.transactionData.map(tx => (tx.id().toString, b))).toMap

          val requestedIds = ((validBlocks ++ invalidBlocks).flatMap(_.transactionData) ++ unknownTransactions).map(_.id().toString)

          val queryParams = requestedIds.map(id => s"id=$id").mkString("?", "&", "")
          val requestBody = Json.obj("ids" -> requestedIds)

          def validate(response: HttpResponse): Unit = {
            val proofsSize = responseAs[List[JsObject]].size
            proofsSize shouldBe (requestedIds.size - invalidBlocks.map(_.transactionData.size).sum - unknownTransactions.size)
            validateSuccess(txIdsToBlock, response)
          }

          (addressTransactions.transactionProofs _).expects(*).returning(Nil).anyNumberOfTimes()

          Get(routePath(s"/merkleProof$queryParams")) ~> route ~> check {
            validate(response)
          }

          Post(routePath("/merkleProof"), requestBody) ~> route ~> check {
            validate(response)
          }
      }
    }

    "returns error in case of all transactions are filtered" in {
      forAll(invalidBlocksGen) { blocks =>
        val txIdsToBlock = blocks.flatMap(b => b.transactionData.map(tx => (tx.id().toString, b))).toMap

        val queryParams = txIdsToBlock.keySet.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> txIdsToBlock.keySet)

        (addressTransactions.transactionProofs _).expects(*).returning(Nil).anyNumberOfTimes()

        Get(routePath(s"/merkleProof$queryParams")) ~> route ~> check {
          validateFailure(response)
        }

        Post(routePath("/merkleProof"), requestBody) ~> route ~> check {
          validateFailure(response)
        }
      }
    }

    "handles invalid signatures" in {
      val invalidIdsGen = for {
        ids       <- Gen.nonEmptyListOf(randomTransactionGen.map(_.id().toString))
        invalidId <- Gen.nonEmptyListOf(invalidBase58Gen)
      } yield Random.shuffle(ids ++ invalidId)

      forAll(invalidIdsGen) { invalidIds =>
        val queryParams = invalidIds.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> invalidIds)

        Get(routePath(s"/merkleProof$queryParams")) ~> route should produce(InvalidSignature)

        Post(routePath("/merkleProof"), requestBody) ~> route should produce(InvalidSignature)
      }
    }
  }
}

package com.wavesplatform.http

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.ApiError.{InvalidAddress, InvalidSignature, TooBigArrayAllocation}
import com.wavesplatform.api.http.TransactionsApiRoute
import com.wavesplatform.block.Block
import com.wavesplatform.block.merkle.Merkle.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.lang.directives.values.V1
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, FUNCTION_CALL, TRUE}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.{BlockchainSettings, GenesisSettings, RewardsSettings, TestFunctionalitySettings, WalletSettings}
import com.wavesplatform.state.{AssetDescription, Blockchain, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{BlockGen, NoShrink, TestTime, TransactionGen}
import monix.execution.Scheduler
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

  implicit def scheduler: Scheduler = Scheduler.global

  private val wallet              = Wallet(WalletSettings(None, Some("qwerty"), None))
  private val blockchain          = mock[Blockchain]
  private val utx                 = mock[UtxPool]
  private val utxPoolSynchronizer = mock[UtxPoolSynchronizer]
  private val route               = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

  private val invalidBase58Gen = alphaNumStr.map(_ + "0")

  routePath("/calculateFee") - {
    "transfer with Waves fee" - {
      "TransferTransaction" in {
        val sender: PublicKey = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "senderPublicKey" -> Base58.encode(sender),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 100)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)
        (blockchain.settings _).expects().returning(BlockchainSettings('T', featuresSettings, GenesisSettings.TESTNET, RewardsSettings.TESTNET))

        val route = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 100000
        }
      }

      "MassTransferTransaction" in {
        val sender: PublicKey = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 11,
          "version"         -> 1,
          "senderPublicKey" -> Base58.encode(sender),
          "transfers" -> Json.arr(
            Json.obj(
              "recipient" -> accountGen.sample.get.toAddress,
              "amount"    -> 1000000
            ),
            Json.obj(
              "recipient" -> accountGen.sample.get.toAddress,
              "amount"    -> 2000000
            )
          )
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 100)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)
        (blockchain.settings _).expects().returning(BlockchainSettings('T', featuresSettings, GenesisSettings.TESTNET, RewardsSettings.TESTNET))

        val route = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

        Post(routePath("/calculateFee"), transferTx) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          (responseAs[JsObject] \ "feeAssetId").asOpt[String] shouldBe empty
          (responseAs[JsObject] \ "feeAmount").as[Long] shouldEqual 200000
        }
      }
    }

    "transfer with Asset fee" - {
      "without sponsorship" in {
        val assetId: ByteStr  = issueGen.sample.get.assetId
        val sender: PublicKey = accountGen.sample.get
        val transferTx = Json.obj(
          "type"            -> 4,
          "version"         -> 2,
          "amount"          -> 1000000,
          "feeAssetId"      -> assetId.toString,
          "senderPublicKey" -> Base58.encode(sender),
          "recipient"       -> accountGen.sample.get.toAddress
        )

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 100)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(1).anyNumberOfTimes()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).anyNumberOfTimes()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)
        (blockchain.settings _).expects().returning(BlockchainSettings('T', featuresSettings, GenesisSettings.TESTNET, RewardsSettings.TESTNET))

        val route = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

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

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 0)
        )
        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(featuresSettings.featureCheckBlocksPeriod).once()
        (blockchain.hasScript _).expects(sender.toAddress).returning(false).once()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)
        (blockchain.settings _).expects().returning(BlockchainSettings('T', featuresSettings, GenesisSettings.TESTNET, RewardsSettings.TESTNET))
        (blockchain.assetDescription _)
          .expects(assetId)
          .returning(
            Some(
              AssetDescription(
                issuer = accountGen.sample.get,
                name = "foo",
                description = "bar",
                decimals = 8,
                reissuable = false,
                totalVolume = Long.MaxValue,
                lastUpdatedAt = Height @@ 0,
                script = None,
                sponsorship = 5
              )
            )
          )
          .anyNumberOfTimes()

        val route = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

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

        val featuresSettings = TestFunctionalitySettings.Enabled.copy(
          preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures + (BlockchainFeatures.FeeSponsorship.id -> 0)
        )

        val blockchain = mock[Blockchain]
        (blockchain.height _).expects().returning(featuresSettings.featureCheckBlocksPeriod).once()
        (blockchain.hasScript _).expects(sender.toAddress).returning(true).once()
        (blockchain.activatedFeatures _).expects().returning(featuresSettings.preActivatedFeatures)
        (blockchain.settings _).expects().returning(BlockchainSettings('T', featuresSettings, GenesisSettings.TESTNET, RewardsSettings.TESTNET))
        (blockchain.assetDescription _)
          .expects(assetId)
          .returning(
            Some(
              AssetDescription(
                issuer = accountGen.sample.get,
                name = "foo",
                description = "bar",
                decimals = 8,
                reissuable = false,
                totalVolume = Long.MaxValue,
                lastUpdatedAt = Height @@ 0,
                script = Some(ExprScript(V1, TRUE, checkSize = false).explicitGet()),
                sponsorship = 5
              )
            )
          )
          .anyNumberOfTimes()

        val route = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

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
      def routeGen: Gen[Route] =
        Gen.const({
          val b = mock[Blockchain]
          TransactionsApiRoute(restAPISettings, wallet, b, utx, utxPoolSynchronizer, new TestTime).route
        })

      "address and limit" in {
        forAll(routeGen, addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect")) {
          case (r, address, limit) =>
            Get(routePath(s"/address/$address/limit/$limit")) ~> r ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }

      "address, limit and after" in {
        forAll(routeGen, addressGen, choose(1, MaxTransactionsPerRequest).label("limitCorrect"), bytes32StrGen) {
          case (r, address, limit, txId) =>
            Get(routePath(s"/address/$address/limit/$limit?after=$txId")) ~> r ~> check {
              status shouldEqual StatusCodes.OK
            }
        }
      }
    }
  }

  routePath("/info/{signature}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx     <- randomTransactionGen
        height <- posNum[Int]
      } yield (tx, height)

      forAll(txAvailability) {
        case (tx, height) =>
          (blockchain.transactionInfo _).expects(tx.id()).returning(Some((height, tx))).once()
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
        (utx.all _).expects().returns(txs).once()
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
        (utx.size _).expects().returns(txs.size).once()
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
      }
    }
  }

  routePath("/unconfirmed/info/{signature}") - {
    "handles invalid signature" in {
      forAll(invalidBase58Gen) { invalidBase58 =>
        Get(routePath(s"/unconfirmed/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/unconfirmed/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/unconfirmed/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      forAll(randomTransactionGen) { tx =>
        (utx.transactionById _).expects(tx.id()).returns(Some(tx)).once()
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
          "version"    -> 1,
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
    import com.wavesplatform.block.BlockMerkleOps

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

    def prepareBlockchain(blocks: List[Block]): Blockchain = { // resetting blockchain for each property check iteration
      val blockchain        = mock[Blockchain]
      val heightToBlock     = blocks.zipWithIndex.map { case (b, h) => (h + 1, b) }.toMap
      val txIdToHeightAndTx = heightToBlock.flatMap { case (h, b) => b.transactionData.map(tx => (tx.id(), (h, tx))) }
      (blockchain.transactionInfo _).expects(*).onCall((x: ByteStr) => txIdToHeightAndTx.get(x)).anyNumberOfTimes()
      (blockchain.blockBytes(_: Int)).expects(*).onCall((h: Int) => heightToBlock.get(h).map(_.bytes())).anyNumberOfTimes()
      blockchain
    }

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
        val blockchain = prepareBlockchain(blocks)
        val route      = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

        val txIdsToBlock = blocks.flatMap(b => b.transactionData.map(tx => (tx.id().toString, b))).toMap

        val queryParams = txIdsToBlock.keySet.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> txIdsToBlock.keySet)

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
          val blockchain = prepareBlockchain(validBlocks ++ invalidBlocks)
          val route      = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

          val txIdsToBlock = validBlocks.flatMap(b => b.transactionData.map(tx => (tx.id().toString, b))).toMap

          val requestedIds = ((validBlocks ++ invalidBlocks).flatMap(_.transactionData) ++ unknownTransactions).map(_.id().toString)

          val queryParams = requestedIds.map(id => s"id=$id").mkString("?", "&", "")
          val requestBody = Json.obj("ids" -> requestedIds)

          def validate(response: HttpResponse): Unit = {
            val proofsSize = responseAs[List[JsObject]].size
            proofsSize shouldBe (requestedIds.size - invalidBlocks.map(_.transactionData.size).sum - unknownTransactions.size)
            validateSuccess(txIdsToBlock, response)
          }

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
        val blockchain = prepareBlockchain(blocks)
        val route      = TransactionsApiRoute(restAPISettings, wallet, blockchain, utx, utxPoolSynchronizer, new TestTime).route

        val txIdsToBlock = blocks.flatMap(b => b.transactionData.map(tx => (tx.id().toString, b))).toMap

        val queryParams = txIdsToBlock.keySet.map(id => s"id=$id").mkString("?", "&", "")
        val requestBody = Json.obj("ids" -> txIdsToBlock.keySet)

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

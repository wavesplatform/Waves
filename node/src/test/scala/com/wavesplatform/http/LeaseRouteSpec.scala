package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity}
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.api.common.{CommonAccountsApi, LeaseInfo}
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.{V5, V6}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.state.diffs.ci
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{BinaryDataEntry, Blockchain, Diff, Height, TxMeta}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.utils.EthTxGenerator.Arg
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Asset, TxHelpers, TxVersion}
import com.wavesplatform.utils.SystemTime
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{NTPTime, TestWallet, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.concurrent.Future

class LeaseRouteSpec
    extends RouteSpec("/leasing")
    with TransactionGen
    with RestAPISettingsHelper
    with NTPTime
    with WithDomain
    with TestWallet
    with PathMockFactory {
  private def route(domain: Domain) =
    LeaseApiRoute(
      restAPISettings,
      testWallet,
      domain.blockchain,
      (_, _) => Future.successful(TracedResult(Right(true))),
      ntpTime,
      CommonAccountsApi(() => domain.blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), domain.db, domain.blockchain)
    )

  private def withRoute(f: (Domain, Route) => Unit): Unit =
    withDomain(domainSettingsWithPreactivatedFeatures(BlockchainFeatures.implemented.flatMap(BlockchainFeatures.feature).toSeq*)) { d =>
      f(d, route(d).route)
    }

  private val leaseContract = TestCompiler(V5).compileContract(
    """
      |@Callable(inv)
      |func leaseTo(recipient: ByteVector, amount: Int) = {
      |  let lease = Lease(Address(recipient), amount)
      |  [
      |    lease,
      |    BinaryEntry("leaseId", lease.calculateLeaseId())
      |  ]
      |}
      |
      |@Callable(inv)
      |func cancelLease(id: ByteVector) = {
      |  [
      |    LeaseCancel(id)
      |  ]
      |}
    """.stripMargin
  )

  private def setScriptTransaction(sender: KeyPair) =
    SetScriptTransaction
      .selfSigned(
        TxVersion.V2,
        sender,
        Some(leaseContract),
        0.01.waves,
        ntpTime.getTimestamp()
      )
      .explicitGet()

  private def invokeLeaseCancel(sender: KeyPair, leaseId: ByteStr) =
    Signed.invokeScript(
      TxVersion.V2,
      sender,
      sender.toAddress,
      Some(
        FUNCTION_CALL(
          FunctionHeader.User("cancelLease"),
          List(CONST_BYTESTR(leaseId).explicitGet())
        )
      ),
      Seq.empty,
      0.005.waves,
      Asset.Waves,
      ntpTime.getTimestamp()
    )

  private def leaseCancelTransaction(sender: KeyPair, leaseId: ByteStr) =
    LeaseCancelTransaction.selfSigned(TxVersion.V3, sender, leaseId, 0.001.waves, ntpTime.getTimestamp()).explicitGet()

  private def checkDetails(id: ByteStr, details: LeaseDetails, json: JsObject): Unit = {
    (json \ "id").as[ByteStr] shouldEqual id
    (json \ "originTransactionId").as[ByteStr] shouldEqual details.sourceId
    (json \ "sender").as[String] shouldEqual details.sender.toAddress.toString
    (json \ "amount").as[Long] shouldEqual details.amount
  }

  private def checkActiveLeasesFor(address: AddressOrAlias, route: Route, expectedDetails: Seq[(ByteStr, LeaseDetails)]): Unit =
    Get(routePath(s"/active/$address")) ~> route ~> check {
      val resp = responseAs[Seq[JsObject]]
      resp.size shouldEqual expectedDetails.size
      resp.zip(expectedDetails).foreach { case (json, (id, details)) =>
        checkDetails(id, details, json)
      }
    }

  private def toDetails(lt: LeaseTransaction) = LeaseDetails(lt.sender, lt.recipient, lt.amount, LeaseDetails.Status.Active, lt.id(), 1)

  private def leaseGen(sender: KeyPair, maxAmount: Long, timestamp: Long): Gen[LeaseTransaction] =
    for {
      fee       <- smallFeeGen
      recipient <- accountGen
      amount    <- Gen.chooseNum(1, (maxAmount - fee).max(1))
      version   <- Gen.oneOf(1.toByte, 2.toByte, 3.toByte)
    } yield LeaseTransaction.selfSigned(version, sender, recipient.toAddress, amount, fee, timestamp).explicitGet()

  "returns active leases which were" - {
    val genesisWithLease = for {
      sender  <- accountGen
      genesis <- genesisGeneratorP(sender.toAddress)
      leaseTx <- leaseGen(sender, genesis.amount, ntpTime.correctedTime())
    } yield (sender, genesis, leaseTx)

    "created and cancelled by Lease/LeaseCancel transactions" in forAll(genesisWithLease) { case (sender, genesis, leaseTransaction) =>
      withRoute { (d, r) =>
        d.appendBlock(genesis)
        d.appendBlock(leaseTransaction)
        val expectedDetails = Seq(leaseTransaction.id() -> toDetails(leaseTransaction))
        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)
        }

        d.appendMicroBlock(leaseCancelTransaction(sender, leaseTransaction.id()))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
        }
      }
    }

    "created by LeaseTransaction and canceled by InvokeScriptTransaction" in forAll(genesisWithLease) { case (sender, genesis, leaseTransaction) =>
      withRoute { (d, r) =>
        d.appendBlock(genesis)
        d.appendBlock(leaseTransaction)
        val expectedDetails = Seq(leaseTransaction.id() -> toDetails(leaseTransaction))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, r, expectedDetails)
        }

        d.appendMicroBlock(
          setScriptTransaction(sender),
          invokeLeaseCancel(sender, leaseTransaction.id())
        )

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, r, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, r, Seq.empty)
        }
      }
    }

    val genesisWithSetScriptAndInvoke = for {
      sender    <- accountGen
      genesis   <- genesisGeneratorP(sender.toAddress)
      recipient <- accountGen
    } yield (
      sender,
      genesis,
      setScriptTransaction(sender),
      Signed.invokeScript(
        TxVersion.V2,
        sender,
        sender.toAddress,
        Some(
          FUNCTION_CALL(
            FunctionHeader.User("leaseTo"),
            List(CONST_BYTESTR(ByteStr(recipient.toAddress.bytes)).explicitGet(), CONST_LONG(10_000.waves))
          )
        ),
        Seq.empty,
        0.005.waves,
        Asset.Waves,
        ntpTime.getTimestamp()
      ),
      recipient.toAddress
    )

    "created by InvokeScriptTransaction and canceled by CancelLeaseTransaction" in forAll(genesisWithSetScriptAndInvoke) {
      case (sender, genesis, setScript, invoke, recipient) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis, setScript)
          d.appendBlock(invoke)
          val leaseId = d.blockchain
            .accountData(genesis.recipient, "leaseId")
            .collect { case i: BinaryDataEntry =>
              i.value
            }
            .get
          val expectedDetails = Seq(leaseId -> LeaseDetails(setScript.sender, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
            checkActiveLeasesFor(recipient, r, expectedDetails)
          }

          d.appendMicroBlock(leaseCancelTransaction(sender, leaseId))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
            checkActiveLeasesFor(recipient, r, Seq.empty)
          }
        }
    }

    "created and canceled by InvokeScriptTransaction" in forAll(genesisWithSetScriptAndInvoke) {
      case (sender, genesis, setScript, invoke, recipient) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis, setScript)
          d.appendBlock(invoke)
          val invokeStatus = d.blockchain.transactionMeta(invoke.id()).get.succeeded
          assert(invokeStatus, "Invoke has failed")

          val leaseId = d.blockchain
            .accountData(genesis.recipient, "leaseId")
            .collect { case i: BinaryDataEntry =>
              i.value
            }
            .get
          val expectedDetails = Seq(leaseId -> LeaseDetails(setScript.sender, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
            checkActiveLeasesFor(recipient, r, expectedDetails)
          }

          d.appendMicroBlock(invokeLeaseCancel(sender, leaseId))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
            checkActiveLeasesFor(recipient, r, Seq.empty)
          }
        }
    }

    val genesisWithInvokeExpression = for {
      sender    <- accountGen
      recipient <- accountGen
      genesis   <- genesisGeneratorP(sender.toAddress)
      invokeExp <- invokeExpressionTransactionGen(
        sender,
        TestCompiler(V6).compileFreeCall(
          s"""
             |let lease = Lease(Address(base58'${recipient.toAddress.toString}'), ${10000.waves})
             |[
             |  lease,
             |  BinaryEntry("leaseId", lease.calculateLeaseId())
             |]""".stripMargin
        ),
        0.01.waves
      )
    } yield (
      sender,
      genesis,
      invokeExp,
      recipient.toAddress
    )

    "created by InvokeExpressionTransaction and canceled by CancelLeaseTransaction" in forAll(genesisWithInvokeExpression) {
      case (sender, genesis, invoke, recipient) =>
        withRoute { (d, r) =>
          d.appendBlock(genesis)
          d.appendBlock(invoke)
          val leaseId = d.blockchain
            .accountData(genesis.recipient, "leaseId")
            .collect { case i: BinaryDataEntry =>
              i.value
            }
            .get
          val expectedDetails = Seq(leaseId -> LeaseDetails(sender.publicKey, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, expectedDetails)
            checkActiveLeasesFor(recipient, r, expectedDetails)
          }

          d.appendMicroBlock(leaseCancelTransaction(sender, leaseId))

          d.liquidAndSolidAssert { () =>
            checkActiveLeasesFor(sender.toAddress, r, Seq.empty)
            checkActiveLeasesFor(recipient, r, Seq.empty)
          }
        }
    }

    def genesisWithEthereumInvoke(useInvokeExpression: Boolean) = for {
      sender    <- ethAccountGen
      dApp      <- accountGen
      recipient <- accountGen
      genesisS  <- genesisGeneratorP(dApp.toAddress)
      setScript = setScriptTransaction(dApp)
      invokeEth <-
        if (useInvokeExpression) Gen.const {
          val args = List(CONST_BYTESTR(ByteStr(recipient.toAddress.bytes)).explicitGet(), CONST_LONG(10000.waves))
          ci.toEthInvokeExpression(setScript, sender, "leaseTo", args)
        }
        else
          ethereumInvokeTransactionGen(
            sender,
            dApp,
            "leaseTo",
            Seq(Arg.Bytes(ByteStr(recipient.toAddress.bytes)), Arg.Integer(10000.waves))
          )
      invokeLeaseCancel = (leaseId: ByteStr) => {
        ci.toEthInvokeExpression(setScript, sender, "cancelLease", List(CONST_BYTESTR(leaseId).explicitGet()))
      }
      genesisE <- genesisGeneratorP(invokeEth.sender.toAddress)
    } yield (
      Seq(genesisS, genesisE, setScript),
      dApp,
      invokeEth,
      invokeLeaseCancel,
      recipient.toAddress
    )

    "created by EthereumTransaction and canceled by LeaseCancelTransaction for InvokeScript and EthereumTransaction for InvokeExpression" in
      Seq(true, false)
        .foreach { useInvokeExpression =>
          val (preparingTxs, dAppKp, invoke, invokeEthCancel, recipient) = genesisWithEthereumInvoke(useInvokeExpression).sample.get

          val leaseCancel   = if (useInvokeExpression) invokeEthCancel else leaseCancelTransaction(dAppKp, _)
          val leaseSenderPk = if (useInvokeExpression) invoke.sender else dAppKp.publicKey
          val leaseSender   = leaseSenderPk.toAddress

          withRoute { (d, r) =>
            d.appendBlock(preparingTxs: _*)
            d.appendBlock(invoke)

            val leaseId         = d.blockchain.accountData(leaseSender, "leaseId").collect { case i: BinaryDataEntry => i.value }.get
            val expectedDetails = Seq(leaseId -> LeaseDetails(leaseSenderPk, recipient, 10_000.waves, LeaseDetails.Status.Active, invoke.id(), 1))

            d.liquidAndSolidAssert { () =>
              checkActiveLeasesFor(leaseSender, r, expectedDetails)
              checkActiveLeasesFor(recipient, r, expectedDetails)
            }

            d.appendMicroBlock(leaseCancel(leaseId))

            d.liquidAndSolidAssert { () =>
              checkActiveLeasesFor(leaseSender, r, Seq.empty)
              checkActiveLeasesFor(recipient, r, Seq.empty)
            }
          }
        }

    val nestedInvocation = for {
      proxy     <- accountGen
      target    <- accountGen
      g1        <- genesisGeneratorP(proxy.toAddress)
      g2        <- genesisGeneratorP(target.toAddress)
      recipient <- accountGen
    } yield (
      (proxy, target, recipient.toAddress),
      Seq(
        g1,
        g2,
        setScriptTransaction(target),
        SetScriptTransaction
          .selfSigned(
            TxVersion.V2,
            proxy,
            Some(TestCompiler(V5).compileContract("""
                                                    |{-# STDLIB_VERSION 4 #-}
                                                    |{-# CONTENT_TYPE DAPP #-}
                                                    |{-# SCRIPT_TYPE ACCOUNT #-}
                                                    |
                                                    |@Callable(inv)
                                                    |func callProxy(targetDapp: ByteVector, recipient: ByteVector, amount: Int) = {
                                                    |  strict result = invoke(Address(targetDapp), "leaseTo", [recipient, amount], [])
                                                    |  []
                                                    |}
                                                  """.stripMargin)),
            0.01.waves,
            ntpTime.getTimestamp()
          )
          .explicitGet()
      )
    )

    "created by nested invocations" in forAll(nestedInvocation) { case ((proxy, target, recipient), genesisTransactions) =>
      withRoute { (d, r) =>
        d.appendBlock(genesisTransactions*)
        val ist = Signed.invokeScript(
          TxVersion.V2,
          proxy,
          proxy.toAddress,
          Some(
            FUNCTION_CALL(
              FunctionHeader.User("callProxy"),
              List(
                CONST_BYTESTR(ByteStr(target.toAddress.bytes)).explicitGet(),
                CONST_BYTESTR(ByteStr(recipient.bytes)).explicitGet(),
                CONST_LONG(10_000.waves)
              )
            )
          ),
          Seq.empty,
          0.005.waves,
          Asset.Waves,
          ntpTime.getTimestamp()
        )

        d.appendBlock(ist)
        val leaseId = d.blockchain
          .accountData(target.toAddress, "leaseId")
          .collect { case i: BinaryDataEntry =>
            i.value
          }
          .get

        val expectedDetails = Seq(leaseId -> LeaseDetails(target.publicKey, recipient, 10_000.waves, LeaseDetails.Status.Active, ist.id(), 1))

        d.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(target.toAddress, r, expectedDetails)
          checkActiveLeasesFor(recipient, r, expectedDetails)
        }
      }
    }
  }

  routePath("/info") in {
    val blockchain = stub[Blockchain]
    val commonApi  = stub[CommonAccountsApi]

    val route = LeaseApiRoute(restAPISettings, stub[Wallet], blockchain, stub[TransactionPublisher], SystemTime, commonApi).route

    val lease       = TxHelpers.lease()
    val leaseCancel = TxHelpers.leaseCancel(lease.id())
    (blockchain.transactionInfo _).when(lease.id()).returning(Some(TxMeta(Height(1), true, 0L) -> lease))
    (commonApi.leaseInfo _)
      .when(lease.id())
      .returning(
        Some(
          LeaseInfo(
            lease.id(),
            lease.id(),
            lease.sender.toAddress,
            lease.recipient.asInstanceOf[Address],
            lease.amount,
            1,
            LeaseInfo.Status.Canceled,
            Some(2),
            Some(leaseCancel.id())
          )
        )
      )
    (commonApi.leaseInfo _).when(*).returning(None)

    Get(routePath(s"/info/${lease.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response should matchJson(s"""{
                                   |  "id" : "${lease.id()}",
                                   |  "originTransactionId" : "${lease.id()}",
                                   |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                   |  "amount" : 1000000000,
                                   |  "height" : 1,
                                   |  "status" : "canceled",
                                   |  "cancelHeight" : 2,
                                   |  "cancelTransactionId" : "${leaseCancel.id()}"
                                   |}""".stripMargin)
    }

    val leasesListJson = Json.parse(s"""[{
                                       |  "id" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                       |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                       |  "amount" : 1000000000,
                                       |  "height" : 1,
                                       |  "status" : "canceled",
                                       |  "cancelHeight" : 2,
                                       |  "cancelTransactionId" : "${leaseCancel.id()}"
                                       |},
                                       {
                                       |  "id" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                       |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                       |  "amount" : 1000000000,
                                       |  "height" : 1,
                                       |  "status" : "canceled",
                                       |  "cancelHeight" : 2,
                                       |  "cancelTransactionId" : "${leaseCancel.id()}"
                                       |}]""".stripMargin)

    Get(routePath(s"/info?id=${lease.id()}&id=${lease.id()}")) ~> route ~> check {
      val response = responseAs[JsArray]
      response should matchJson(leasesListJson)
    }

    Post(
      routePath(s"/info"),
      HttpEntity(ContentTypes.`application/json`, Json.obj("ids" -> Seq(lease.id().toString, lease.id().toString)).toString())
    ) ~> route ~> check {
      val response = responseAs[JsArray]
      response should matchJson(leasesListJson)
    }

    Post(
      routePath(s"/info"),
      HttpEntity(
        ContentTypes.`application/json`,
        Json.obj("ids" -> (0 to restAPISettings.transactionsByAddressLimit).map(_ => lease.id().toString)).toString()
      )
    ) ~> route ~> check {
      val response = responseAs[JsObject]
      response should matchJson("""{
                                  |  "error" : 10,
                                  |  "message" : "Too big sequence requested: max limit is 10000 entries"
                                  |}""".stripMargin)
    }

    Post(
      routePath(s"/info"),
      FormData("id" -> lease.id().toString, "id" -> lease.id().toString)
    ) ~> route ~> check {
      val response = responseAs[JsArray]
      response should matchJson(leasesListJson)
    }

    Get(routePath(s"/info?id=nonvalid&id=${leaseCancel.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response should matchJson(s"""
                                   |{
                                   |  "error" : 116,
                                   |  "message" : "Request contains invalid IDs. nonvalid, ${leaseCancel.id()}",
                                   |  "ids" : [ "nonvalid", "${leaseCancel.id()}" ]
                                   |}""".stripMargin)
    }
  }
}

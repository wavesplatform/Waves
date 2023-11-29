package com.wavesplatform.http

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity}
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.RouteTimeout
import com.wavesplatform.api.http.leasing.LeaseApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.{V5, V6}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CONST_LONG}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.{BinaryDataEntry, Blockchain, LeaseDetails, LeaseStaticInfo}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.EthTxGenerator.Arg
import com.wavesplatform.transaction.TxHelpers.signer
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{Authorized, EthTxGenerator, Transaction, TxHelpers, TxPositiveAmount, TxVersion}
import com.wavesplatform.utils.SharedSchedulerMixin
import org.scalactic.source.Position
import org.scalatest.OptionValues
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.concurrent.duration.*

class LeaseRouteSpec extends RouteSpec("/leasing") with OptionValues with RestAPISettingsHelper with SharedDomain with SharedSchedulerMixin {
  private val richAccount = TxHelpers.signer(200)

  override def genesisBalances: Seq[WithState.AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 500_000.waves))
  override def settings: WavesSettings                         = DomainPresets.ContinuationTransaction

  private val route =
    seal(
      LeaseApiRoute(
        restAPISettings,
        domain.wallet,
        domain.blockchain,
        DummyTransactionPublisher.accepting,
        ntpTime,
        CommonAccountsApi(() => domain.blockchainUpdater.snapshotBlockchain, domain.rdb, domain.blockchain),
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route
    )

  private val leaseContract = TestCompiler(V5)
    .compileContract(
      """
        | @Callable(inv)
        | func leaseTo(recipient: ByteVector, amount: Int) = {
        |   let lease = Lease(Address(recipient), amount)
        |   [
        |     lease,
        |     BinaryEntry("leaseId", lease.calculateLeaseId())
        |   ]
        | }
        |
        | @Callable(inv)
        | func cancelLease(id: ByteVector) = {
        |   [
        |     LeaseCancel(id)
        |   ]
        | }
      """.stripMargin
    )

  private def setScriptTransaction(sender: KeyPair) =
    TxHelpers.setScript(sender, leaseContract)

  private def invokeLeaseCancel(sender: KeyPair, leaseId: ByteStr) =
    TxHelpers.invoke(sender.toAddress, Some("cancelLease"), Seq(CONST_BYTESTR(leaseId).explicitGet()))

  private def leaseCancelTransaction(sender: KeyPair, leaseId: ByteStr) =
    TxHelpers.leaseCancel(leaseId, sender, version = TxVersion.V3)

  private def checkDetails(id: ByteStr, details: LeaseDetails, json: JsObject): Unit = {
    (json \ "id").as[ByteStr] shouldEqual id
    (json \ "originTransactionId").as[ByteStr] shouldEqual details.sourceId
    (json \ "sender").as[String] shouldEqual details.sender.toAddress.toString
    (json \ "amount").as[Long] shouldEqual details.amount.value
  }

  private def checkActiveLeasesFor(address: AddressOrAlias, route: Route, expectedDetails: Seq[(ByteStr, LeaseDetails)])(implicit
      pos: Position
  ): Unit =
    Get(routePath(s"/active/$address")) ~> route ~> check {
      val resp = responseAs[Seq[JsObject]]
      resp.size shouldEqual expectedDetails.size
      resp.zip(expectedDetails).foreach { case (json, (id, details)) =>
        checkDetails(id, details, json)
      }
    }

  private def toDetails(lt: LeaseTransaction, blockchain: Blockchain) =
    LeaseDetails(
      LeaseStaticInfo(lt.sender, blockchain.resolveAlias(lt.recipient).explicitGet(), lt.amount, lt.id(), blockchain.height),
      LeaseDetails.Status.Active
    )

  "returns active leases which were" - {
    val transactionVersions = Table("lease transaction version", 1.toByte, 2.toByte, 3.toByte)

    "created and cancelled by Lease/LeaseCancel transactions" in {
      val lessor         = TxHelpers.signer(201)
      val leaseRecipient = TxHelpers.address(202)

      domain.appendBlock(TxHelpers.transfer(richAccount, lessor.toAddress, 30.006.waves))

      forAll(transactionVersions) { v =>
        val leaseTransaction = TxHelpers.lease(lessor, leaseRecipient, version = v)
        val expectedDetails  = Seq(leaseTransaction.id() -> toDetails(leaseTransaction, domain.blockchain))

        domain.appendBlock(leaseTransaction)

        domain.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, route, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, route, expectedDetails)
        }

        domain.appendMicroBlock(TxHelpers.leaseCancel(leaseTransaction.id(), lessor))

        domain.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, route, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, route, Seq.empty)
        }
      }
    }

    "created by LeaseTransaction and canceled by InvokeScriptTransaction" in {
      val lessor         = TxHelpers.signer(202)
      val leaseRecipient = TxHelpers.address(203)

      domain.appendBlock(TxHelpers.transfer(richAccount, lessor.toAddress, 35.waves))
      forAll(transactionVersions) { v =>
        val leaseTransaction = TxHelpers.lease(lessor, leaseRecipient, version = v)
        domain.appendBlock(leaseTransaction)
        val expectedDetails = Seq(leaseTransaction.id() -> toDetails(leaseTransaction, domain.blockchain))

        domain.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, route, expectedDetails)
          checkActiveLeasesFor(leaseTransaction.recipient, route, expectedDetails)
        }

        domain.appendMicroBlock(
          setScriptTransaction(lessor),
          invokeLeaseCancel(lessor, leaseTransaction.id())
        )

        domain.liquidAndSolidAssert { () =>
          checkActiveLeasesFor(leaseTransaction.sender.toAddress, route, Seq.empty)
          checkActiveLeasesFor(leaseTransaction.recipient, route, Seq.empty)
        }
      }
    }

    val dappAddress    = TxHelpers.signer(200)
    val leaseRecipient = TxHelpers.address(201)

    def setScriptAndInvoke =
      (
        setScriptTransaction(dappAddress),
        TxHelpers.invoke(
          dappAddress.toAddress,
          Some("leaseTo"),
          Seq(CONST_BYTESTR(ByteStr(leaseRecipient.bytes)).explicitGet(), CONST_LONG(10_000.waves)),
          invoker = dappAddress
        )
      )

    "created by InvokeScriptTransaction and canceled by CancelLeaseTransaction" in {
      val (setScript, invoke) = setScriptAndInvoke
      domain.appendBlock(TxHelpers.transfer(richAccount, dappAddress.toAddress, 20_000.waves), setScript, invoke)
      val leaseId = domain.blockchain
        .accountData(dappAddress.toAddress, "leaseId")
        .collect { case i: BinaryDataEntry =>
          i.value
        }
        .value

      val expectedDetails = Seq(
        leaseId -> LeaseDetails(
          LeaseStaticInfo(dappAddress.publicKey, leaseRecipient, TxPositiveAmount(10_000_00000000L), invoke.id(), 1),
          LeaseDetails.Status.Active
        )
      )

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(dappAddress.toAddress, route, expectedDetails)
        checkActiveLeasesFor(leaseRecipient, route, expectedDetails)
      }

      domain.appendMicroBlock(leaseCancelTransaction(dappAddress, leaseId))

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(dappAddress.toAddress, route, Seq.empty)
        checkActiveLeasesFor(leaseRecipient, route, Seq.empty)
      }
    }

    "created and canceled by InvokeScriptTransaction" in {
      val (setScript, invoke) = setScriptAndInvoke

      domain.appendBlock(TxHelpers.transfer(richAccount, dappAddress.toAddress, 20_000.waves), setScript, invoke)
      val invokeStatus = domain.blockchain.transactionMeta(invoke.id()).get.status
      assert(invokeStatus == Status.Succeeded, "Invoke has failed")

      val leaseId = domain.blockchain
        .accountData(dappAddress.toAddress, "leaseId")
        .collect { case i: BinaryDataEntry =>
          i.value
        }
        .get
      val expectedDetails =
        Seq(
          leaseId -> LeaseDetails(
            LeaseStaticInfo(dappAddress.publicKey, leaseRecipient, TxPositiveAmount(10_000_00000000L), invoke.id(), 1),
            LeaseDetails.Status.Active
          )
        )

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(dappAddress.toAddress, route, expectedDetails)
        checkActiveLeasesFor(leaseRecipient, route, expectedDetails)
      }

      domain.appendMicroBlock(invokeLeaseCancel(dappAddress, leaseId))

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(dappAddress.toAddress, route, Seq.empty)
        checkActiveLeasesFor(leaseRecipient, route, Seq.empty)
      }
    }

    "created by InvokeExpressionTransaction and canceled by CancelLeaseTransaction" in {
      val sender    = TxHelpers.signer(210)
      val recipient = TxHelpers.address(211)

      val invoke = TxHelpers.invokeExpression(
        TestCompiler(V6).compileFreeCall(
          s"""
             |let lease = Lease(Address(base58'${recipient.toString}'), ${10000.waves})
             |[
             |  lease,
             |  BinaryEntry("leaseId", lease.calculateLeaseId())
             |]""".stripMargin
        ),
        sender
      )

      domain.appendBlock(TxHelpers.transfer(richAccount, sender.toAddress, 10001.waves), invoke)
      val leaseId = domain.blockchain
        .accountData(sender.toAddress, "leaseId")
        .collect { case i: BinaryDataEntry =>
          i.value
        }
        .get
      val expectedDetails =
        Seq(
          leaseId -> LeaseDetails(
            LeaseStaticInfo(sender.publicKey, recipient, TxPositiveAmount(10_000_00000000L), invoke.id(), 1),
            LeaseDetails.Status.Active
          )
        )

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(sender.toAddress, route, expectedDetails)
        checkActiveLeasesFor(recipient, route, expectedDetails)
      }

      domain.appendMicroBlock(leaseCancelTransaction(sender, leaseId))

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(sender.toAddress, route, Seq.empty)
        checkActiveLeasesFor(recipient, route, Seq.empty)
      }
    }

    "created by EthereumTransaction and canceled by CancelLeaseTransaction" in {
      val sender    = signer(211).toEthKeyPair
      val dApp      = signer(212)
      val recipient = TxHelpers.address(213)

      val invoke =
        EthTxGenerator.generateEthInvoke(
          keyPair = sender,
          address = dApp.toAddress,
          funcName = "leaseTo",
          args = Seq(Arg.Bytes(ByteStr(recipient.bytes)), Arg.Integer(10000.waves)),
          payments = Seq.empty
        )

      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(
            sender.toWavesAddress -> 0.005.waves,
            dApp.toAddress        -> 10_0001.waves
          ),
          fee = 0.002.waves
        ),
        setScriptTransaction(dApp),
        invoke
      )
      val leaseId = domain.blockchain
        .accountData(dApp.toAddress, "leaseId")
        .collect { case i: BinaryDataEntry =>
          i.value
        }
        .get
      val expectedDetails =
        Seq(
          leaseId -> LeaseDetails(
            LeaseStaticInfo(dApp.publicKey, recipient, TxPositiveAmount(10_000_00000000L), invoke.id(), 1),
            LeaseDetails.Status.Active
          )
        )

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(dApp.toAddress, route, expectedDetails)
        checkActiveLeasesFor(recipient, route, expectedDetails)
      }

      domain.appendMicroBlock(leaseCancelTransaction(dApp, leaseId))

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(dApp.toAddress, route, Seq.empty)
        checkActiveLeasesFor(recipient, route, Seq.empty)
      }
    }

    "created by nested invocations" in {
      val proxy     = TxHelpers.signer(221)
      val target    = TxHelpers.signer(222)
      val recipient = TxHelpers.signer(223)

      val ist = TxHelpers.invoke(
        proxy.toAddress,
        Some("callProxy"),
        List(
          CONST_BYTESTR(ByteStr(target.toAddress.bytes)).explicitGet(),
          CONST_BYTESTR(ByteStr(recipient.toAddress.bytes)).explicitGet(),
          CONST_LONG(10_000.waves)
        ),
        invoker = proxy
      )

      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(
            proxy.toAddress  -> 1.waves,
            target.toAddress -> 10_001.waves
          ),
          fee = 0.002.waves
        ),
        setScriptTransaction(target),
        TxHelpers.setScript(
          proxy,
          TestCompiler(V5)
            .compileContract("""@Callable(inv)
                               |func callProxy(targetDapp: ByteVector, recipient: ByteVector, amount: Int) = {
                               |  strict result = invoke(Address(targetDapp), "leaseTo", [recipient, amount], [])
                               |  []
                               |}
                               |""".stripMargin)
        ),
        ist
      )

      val leaseId = domain.blockchain
        .accountData(target.toAddress, "leaseId")
        .collect { case i: BinaryDataEntry =>
          i.value
        }
        .get

      val expectedDetails =
        Seq(
          leaseId -> LeaseDetails(
            LeaseStaticInfo(target.publicKey, recipient.toAddress, TxPositiveAmount(10_000_00000000L), ist.id(), 1),
            LeaseDetails.Status.Active
          )
        )

      domain.liquidAndSolidAssert { () =>
        checkActiveLeasesFor(target.toAddress, route, expectedDetails)
        checkActiveLeasesFor(recipient.toAddress, route, expectedDetails)
      }
    }
  }

  "returns leases created by invoke only for lease sender or recipient" in {
    val invoker         = TxHelpers.signer(231)
    val dApp1           = TxHelpers.signer(232)
    val dApp2           = TxHelpers.signer(233)
    val leaseRecipient1 = TxHelpers.signer(234)
    val leaseRecipient2 = TxHelpers.signer(235)

    val leaseAmount1 = 1
    val leaseAmount2 = 2

    val dAppScript1 = TestCompiler(V5)
      .compileContract(
        s"""
           |@Callable(i)
           |func foo() = {
           |  strict inv = invoke(Address(base58'${dApp2.toAddress}'), "bar", [], [])
           |  let lease = Lease(Address(base58'${leaseRecipient1.toAddress}'), 1)
           |  [lease, BinaryEntry("leaseId", lease.calculateLeaseId())]
           |}
         """.stripMargin
      )

    val dAppScript2 = TestCompiler(V5)
      .compileContract(
        s"""
           |@Callable(i)
           |func bar() = {
           |  let lease = Lease(Address(base58'${leaseRecipient2.toAddress}'), 2)
           |  [lease, BinaryEntry("leaseId", lease.calculateLeaseId())]
           |}
         """.stripMargin
      )

    def checkForInvoke(invokeTx: Transaction & Authorized): Unit = {
      def getLeaseId(address: Address) =
        domain.blockchain
          .accountData(address, "leaseId")
          .collect { case i: BinaryDataEntry =>
            i.value
          }
          .get

      domain.appendBlock(
        TxHelpers.massTransfer(
          richAccount,
          Seq(
            invoker.toAddress                   -> 1.waves,
            dApp1.toAddress                     -> 1.waves,
            dApp2.toAddress                     -> 1.waves,
            invoker.toEthKeyPair.toWavesAddress -> 1.waves
          ),
          fee = 0.005.waves
        ),
        TxHelpers.setScript(dApp1, dAppScript1),
        TxHelpers.setScript(dApp2, dAppScript2)
      )

      domain.appendBlock(invokeTx)

      val lease1Id = getLeaseId(dApp1.toAddress)
      val leaseDetails1 = Seq(
        lease1Id -> LeaseDetails(
          LeaseStaticInfo(dApp1.publicKey, leaseRecipient1.toAddress, TxPositiveAmount.unsafeFrom(leaseAmount1), invokeTx.id(), 3),
          LeaseDetails.Status.Active
        )
      )
      val lease2Id = getLeaseId(dApp2.toAddress)
      val leaseDetails2 = Seq(
        lease2Id -> LeaseDetails(
          LeaseStaticInfo(dApp2.publicKey, leaseRecipient2.toAddress, TxPositiveAmount.unsafeFrom(leaseAmount2), invokeTx.id(), 3),
          LeaseDetails.Status.Active
        )
      )

      checkActiveLeasesFor(invokeTx.sender.toAddress, route, Seq.empty)
      checkActiveLeasesFor(dApp1.toAddress, route, leaseDetails1)
      checkActiveLeasesFor(dApp2.toAddress, route, leaseDetails2)
      checkActiveLeasesFor(leaseRecipient1.toAddress, route, leaseDetails1)
      checkActiveLeasesFor(leaseRecipient2.toAddress, route, leaseDetails2)

      domain.appendBlock(
        TxHelpers.leaseCancel(lease1Id, dApp1),
        TxHelpers.leaseCancel(lease2Id, dApp2)
      )
    }

    checkForInvoke(TxHelpers.invoke(dApp1.toAddress, Some("foo"), invoker = invoker))
    checkForInvoke(EthTxGenerator.generateEthInvoke(invoker.toEthKeyPair, dApp1.toAddress, "foo", Seq.empty, Seq.empty))
  }

  routePath("/info") in {

    val lease       = TxHelpers.lease()
    val leaseCancel = TxHelpers.leaseCancel(lease.id())
    domain.appendBlock(lease)
    val leaseHeight = domain.blockchain.height
    domain.appendBlock(leaseCancel)
    val leaseCancelHeight = domain.blockchain.height

    Get(routePath(s"/info/${lease.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response should matchJson(s"""{
                                   |  "id" : "${lease.id()}",
                                   |  "originTransactionId" : "${lease.id()}",
                                   |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                   |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                   |  "amount" : 1000000000,
                                   |  "height" : $leaseHeight,
                                   |  "status" : "canceled",
                                   |  "cancelHeight" : $leaseCancelHeight,
                                   |  "cancelTransactionId" : "${leaseCancel.id()}"
                                   |}""".stripMargin)
    }

    val leasesListJson = Json.parse(s"""[{
                                       |  "id" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                       |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                       |  "amount" : 1000000000,
                                       |  "height" : $leaseHeight,
                                       |  "status" : "canceled",
                                       |  "cancelHeight" : $leaseCancelHeight,
                                       |  "cancelTransactionId" : "${leaseCancel.id()}"
                                       |},
                                       {
                                       |  "id" : "${lease.id()}",
                                       |  "originTransactionId" : "${lease.id()}",
                                       |  "sender" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                       |  "recipient" : "3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC",
                                       |  "amount" : 1000000000,
                                       |  "height" : $leaseHeight,
                                       |  "status" : "canceled",
                                       |  "cancelHeight" : $leaseCancelHeight,
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

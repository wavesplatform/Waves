package com.wavesplatform.http

import cats.syntax.option.*
import com.wavesplatform.TestValues.commitToGenerationFee
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.{GeneratorsApiRoute, RouteTimeout}
import com.wavesplatform.block.{BlockEndorsement, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.{WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{GenerationPeriod, GeneratorIndex, Height, diffs}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import monix.execution.Scheduler.global
import org.apache.pekko.http.scaladsl.model.StatusCodes.{NotFound, OK}
import org.scalactic.source.Position
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.annotation.targetName
import scala.concurrent.duration.*

/** Scenario:
  * 1. Activate finalization on 2, generation period length is 3
  * 2. Commit on 2
  * 3. Append blocks up to first period start height on 6
  * 4. Issue conflicting endorsement on 6
  * 5. Commit on 9
  * 6. Append blocks up to next period start height on 9
  */
class GeneratorsApiRouteSpec extends RouteSpec("/generators") with WithDomain {
  private val activationHeight       = Height(2)
  private val generationPeriodLength = 3
  private val defaultSettings: WavesSettings = {
    val orig = DomainPresets.DeterministicFinality.setFeaturesHeight(BlockchainFeatures.DeterministicFinality -> activationHeight.toInt)
    orig.copy(
      blockchainSettings = orig.blockchainSettings.copy(
        functionalitySettings = orig.blockchainSettings.functionalitySettings.copy(generationPeriodLength = generationPeriodLength)
      )
    )
  }

  private val generators = Seq(TxHelpers.signer(1000), TxHelpers.signer(1001), TxHelpers.defaultSigner)

  private val Seq(validGenerator, conflictingGenerator, miner)             = generators
  private val Seq(validGeneratorAddr, conflictingGeneratorAddr, minerAddr) = generators.map(_.toAddress.toString)

  private val depositAndFee = CommitToGenerationTransaction.DepositInWavelets + commitToGenerationFee
  private val initBalance   = diffs.ENOUGH_AMT + depositAndFee

  private val defaultInitBalances: Seq[WithState.AddrWithBalance] = generators.map(x => AddrWithBalance(x.toAddress, initBalance))

  private val period1 = GenerationPeriod(
    start = Height(activationHeight.toInt + 1 + generationPeriodLength),
    activation = activationHeight,
    length = generationPeriodLength
  )
  private val period2 = period1.next
  private val period3 = period2.next

  "/generators/at/{height}" - {
    "before activation" in test { d =>
      d.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality) shouldBe false

      d.checkAt() { status shouldBe NotFound }
      d.checkAt(period1.start) { status shouldBe NotFound }
      d.checkAt(period2.start) { status shouldBe NotFound }
    }

    "before commitments" in test { d =>
      d.appendBlock()
      d.blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality) shouldBe true

      d.checkAt(1) { status shouldBe NotFound } // Before activation
      d.checkAt() { jsonBodyIsEmpty() }
      d.checkAt(period1.start) { jsonBodyIsEmpty() }
      d.checkAt(period2.start) { status shouldBe NotFound }
    }

    "committed to the period 1" in test { d =>
      d.appendBlock()
      val txIds = d.commit(generators*)

      d.checkAt(1) { status shouldBe NotFound } // Before activation
      d.checkAt(2) { jsonBodyIsEmpty() }
      d.checkAt() { jsonBodyIsEmpty() }

      d.checkAt(period1.start) {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds(0).toString
          ),
          Json.obj(
            "address"       -> conflictingGeneratorAddr,
            "transactionId" -> txIds(1).toString
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds(2).toString
          )
        )
      }

      d.checkAt(period2.start) { status shouldBe NotFound }
    }

    "on start height of period1" in test { d =>
      d.appendBlock()
      val txIds                   = d.commit(generators*)
      val minerGeneratingBalance2 = d.effBalance(miner.toAddress)

      (3 to 7).foreach { _ =>
        d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner))
      }
      val minerGeneratingBalance6 = d.effBalance(miner.toAddress)

      d.checkAt(period1.start) {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds(0).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> conflictingGeneratorAddr,
            "transactionId" -> txIds(1).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds(2).toString,
            "balance"       -> minerGeneratingBalance2
          )
        )
      }

      d.checkAt() {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds(0).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> conflictingGeneratorAddr,
            "transactionId" -> txIds(1).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds(2).toString,
            "balance"       -> minerGeneratingBalance6
          )
        )
      }

      d.checkAt(period2.start) { jsonBodyIsEmpty() }
      d.checkAt(period3.start) { status shouldBe NotFound }
    }

    "one generator is conflicting" in test { d =>
      d.appendBlock()
      val txIds                   = d.commit(generators*)
      val minerGeneratingBalance2 = d.effBalance(miner.toAddress)

      (3 to 7).foreach { _ =>
        d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner))
      }
      val minerGeneratingBalance6 = d.effBalance(miner.toAddress)
      d.appendConflicting()

      d.checkAt(period1.start) {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds(0).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> conflictingGeneratorAddr,
            "transactionId" -> txIds(1).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds(2).toString,
            "balance"       -> minerGeneratingBalance2
          )
        )
      }

      d.checkAt() {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds(0).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"        -> conflictingGeneratorAddr,
            "transactionId"  -> txIds(1).toString,
            "balance"        -> 0,
            "conflictHeight" -> d.blockchain.height
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds(2).toString,
            "balance"       -> minerGeneratingBalance6
          )
        )
      }

      d.checkAt(period2.start) { jsonBodyIsEmpty() }
      d.checkAt(period3.start) { status shouldBe NotFound }
    }

    "on commit to period2" in test { d =>
      d.appendBlock()
      val txIds1 = d.commit(generators*)

      (3 to 7).foreach { _ =>
        d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner))
      }
      val minerGeneratingBalance = d.effBalance(miner.toAddress)
      d.appendConflicting()
      val txIds2 = d.commit(validGenerator, miner)

      d.checkAt() {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds1(0).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"        -> conflictingGeneratorAddr,
            "transactionId"  -> txIds1(1).toString,
            "balance"        -> 0,
            "conflictHeight" -> d.blockchain.height
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds1(2).toString,
            "balance"       -> minerGeneratingBalance
          )
        )
      }

      d.checkAt(period2.start) {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds2(0).toString
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds2(1).toString
          )
        )
      }

      d.checkAt(period3.start) { status shouldBe NotFound }
    }

    "on the period 2" in test { d =>
      d.appendBlock()
      val txIds1                  = d.commit(generators*)
      val minerGeneratingBalance2 = d.effBalance(miner.toAddress)

      (3 to 7).foreach { _ =>
        d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner))
      }
      d.appendConflicting()
      val txIds2 = d.commit(validGenerator, miner)

      (8 to 9).foreach { _ =>
        d.appender.appendBlock(d.createBlock(strictTime = true, generator = miner))
      }
      val minerGeneratingBalance = d.effBalance(miner.toAddress)

      d.checkAt(period1.start) {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds1(0).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> conflictingGeneratorAddr,
            "transactionId" -> txIds1(1).toString,
            "balance"       -> (initBalance - depositAndFee)
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds1(2).toString,
            "balance"       -> minerGeneratingBalance2
          )
        )
      }

      d.checkAt() {
        jsonBodyIs(
          Json.obj(
            "address"       -> validGeneratorAddr,
            "transactionId" -> txIds2(0).toString,
            "balance"       -> (initBalance - 2 * depositAndFee)
          ),
          Json.obj(
            "address"       -> minerAddr,
            "transactionId" -> txIds2(1).toString,
            "balance"       -> minerGeneratingBalance
          )
        )
      }
    }
  }

  private def test(f: Domain => Unit): Unit = withDomain(defaultSettings, balances = defaultInitBalances)(f)

  private def jsonBodyIsEmpty()(using position: Position): Unit = jsonBodyIs()

  private def jsonBodyIs(expectedItems: JsObject*)(using position: Position): Unit = {
    status shouldBe OK
    responseAs[JsArray] should matchJson(JsArray(expectedItems))
  }

  extension (d: Domain) {
    def commit(generators: KeyPair*): Seq[ByteStr] = {
      val generationPeriod = {
        val p = d.blockchain.currentGenerationPeriod.value.next
        if (p.start == Height(d.blockchain.height + 1)) p.next
        else p
      }

      val txs = generators.map(x => TxHelpers.commitToGeneration(generationPeriod.start, sender = x))
      d.appendMicroBlock(d.createMicroBlock(signer = miner.some)(txs*))
      txs.map(_.id())
    }

    def appendConflicting(): Unit = d.appendMicroBlock(
      d.createMicroBlock(
        signer = miner.some,
        finalizationVoting = FinalizationVoting(
          valid = Nil,
          finalizedHeight = Height(1),
          aggregatedEndorsement = None,
          conflict = Vector(
            BlockEndorsement.signed(
              BlsKeyPair(conflictingGenerator.privateKey),
              GeneratorIndex(1),
              finalizedId = TxHelpers.randomBlockId,
              finalizedHeight = Height(1),
              endorsedId = d.blockchain.lastBlockId.value
            )
          )
        ).some
      )(TxHelpers.transfer())
    )

    @targetName("checkAtHeight") def checkAt(height: Height)(f: => Unit): Unit = checkAt(height.toInt)(f)
    def checkAt(height: Int = d.blockchain.height)(f: => Unit): Unit           = Get(s"/generators/at/$height") ~> route ~> check(f)

    def route = seal(
      GeneratorsApiRoute(
        d.settings.restAPISettings,
        d.blockchain,
        d.generatorsApi,
        d.testTime,
        new RouteTimeout(60.seconds)(using global)
      ).route
    )
  }
}

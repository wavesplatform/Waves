package com.wavesplatform.metrics

import com.wavesplatform.transaction.TransactionParsers
import kamon.Kamon
import kamon.metric.instrument.{Counter, Histogram, InstrumentFactory}
import kamon.metric.{EntityRecorderFactoryCompanion, GenericEntityRecorder}

class TxMetrics(instrumentFactory: InstrumentFactory) extends GenericEntityRecorder(instrumentFactory) {

  private val txIdentifiers = TransactionParsers.all.values.toList.map { builder =>
    val txTypeName = builder.typeId match {
      case 1  => "Genesis"
      case 2  => "Payment"
      case 3  => "Issue"
      case 4  => "Transfer"
      case 5  => "Reissue"
      case 6  => "Burn"
      case 7  => "Exchange"
      case 8  => "Lease"
      case 9  => "LeaseCancel"
      case 10 => "Alias"
      case 11 => "MassTransfer"
      case 12 => "Data"
      case 13 => "SetScript"
      case 14 => "SetSponsorship"
    }

    (builder.typeId, txTypeName)
  }.toSet

  private val counters =
    txIdentifiers.map {
      case (typeId, txTypeName) =>
        val counter = Kamon.metrics
          .counter(
            "processed",
            tags = Map(
              "transaction-type" -> txTypeName
            )
          )

        typeId -> counter
    }.toMap

  private val histograms =
    txIdentifiers.map {
      case (typeId, txTypeName) =>
        val histogram = Kamon.metrics
          .histogram(
            "processing-time",
            tags = Map(
              "transaction-type" -> txTypeName
            )
          )

        typeId -> histogram
    }.toMap

  def processed(typeId: Byte): Counter        = counters(typeId)
  def processingTime(typeId: Byte): Histogram = histograms(typeId)
}

object TxMetrics extends EntityRecorderFactoryCompanion[TxMetrics]("tx-processing", new TxMetrics(_))

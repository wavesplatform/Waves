package com.wavesplatform.ride.runner.entrypoints

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Height

case class Heights(lastKnownHardened: Height, working: Height)

object Heights {
  case class Settings(onEmptyStartFrom: Option[Int], functionalitySettings: FunctionalitySettings)

  def calculate(settings: Settings, localHeight: Option[Height], lastHeightAtStart: Height): Heights = {
    val (lastKnownHardenedHeight, workingHeight) = localHeight match {
      case Some(h) =>
        (
          (h - 100 - 1).max(Height(0)),
          lastHeightAtStart.max(h)
        )

      case None =>
        val depth = settings.functionalitySettings.generatingBalanceDepth(lastHeightAtStart.toInt)
        settings.onEmptyStartFrom match {
          case Some(onEmptyStartFrom) =>
            val maximumStartHeight = lastHeightAtStart - depth
            require(
              Height(onEmptyStartFrom) < maximumStartHeight,
              s"onEmptyStartFrom=$onEmptyStartFrom should be < maximumStartHeight=$maximumStartHeight"
            )
            (
              Height(onEmptyStartFrom),
              lastHeightAtStart
            )

          case None =>
            // to guarantee the right generatingBalance
            (
              (lastHeightAtStart - depth - 1).max(Height(0)),
              lastHeightAtStart
            )
        }
    }
    Heights(lastKnownHardenedHeight, workingHeight)
  }
}

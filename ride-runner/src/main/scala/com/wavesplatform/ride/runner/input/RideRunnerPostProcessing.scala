package com.wavesplatform.ride.runner.input

case class RideRunnerPostProcessing(
    enable: Boolean = false,
    method: RideRunnerPostProcessingMethod = PickRideRunnerPostProcessingMethod("result.value._2.value")
)

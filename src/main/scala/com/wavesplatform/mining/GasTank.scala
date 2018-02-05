package com.wavesplatform.mining

import scorex.block.Block
import scorex.transaction.Transaction

trait GasTank {
  def isEmpty: Boolean
  def withdraw(x: Block): Boolean
  def withdraw(x: Transaction): Boolean
  def copy(): GasTank
}

class OneGasTank private(private var restGas: Long, private val estimator: GasEstimator) extends GasTank {
  private var _isEmpty = restGas <= 0
  override def isEmpty: Boolean = _isEmpty

  override def withdraw(x: Block): Boolean = withdraw(estimator.estimate(x))
  override def withdraw(x: Transaction): Boolean = withdraw(estimator.estimate(x))
  private def withdraw(x: Long): Boolean = {
    val updatedRestGas = restGas - x
    if (updatedRestGas <= 0) _isEmpty = true

    val successfully = updatedRestGas >= 0
    if (successfully) restGas = updatedRestGas
    successfully
  }

  override def copy(): OneGasTank = new OneGasTank(restGas, estimator)
}

object OneGasTank {
  def full(estimator: GasEstimator): GasTank = new OneGasTank(estimator.max, estimator)
}

class DoubleGasTank(private val first: GasTank,
                    private val second: GasTank) extends GasTank {
  override def isEmpty: Boolean = first.isEmpty || second.isEmpty

  override def withdraw(x: Block): Boolean = {
    val firstSuccessfully = first withdraw x
    val secondSuccessfully = second withdraw x
    firstSuccessfully && secondSuccessfully
  }

  override def withdraw(x: Transaction): Boolean = {
    val firstSuccessfully = first withdraw x
    val secondSuccessfully = second withdraw x
    firstSuccessfully && secondSuccessfully
  }

  override def copy(): DoubleGasTank = new DoubleGasTank(first.copy(), second.copy())
}

object DoubleGasTank {
  def full(first: GasEstimator, second: GasEstimator): GasTank = new DoubleGasTank(
    first = OneGasTank.full(first),
    second = OneGasTank.full(second)
  )
  def partial(firstRest: GasTank, secondRest: GasTank): GasTank = new DoubleGasTank(firstRest, secondRest)
}

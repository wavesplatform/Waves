package tools

import play.api.libs.json.Json

object FirstDifferentBlock extends App {

  def get(url: String) = scala.io.Source.fromURL(url).mkString

  def blockAt(nodeHttp: String, blockHeight: Int)    = get(nodeHttp + "/blocks/at/" + blockHeight)
  def blockSigAt(nodeHttp: String, blockHeight: Int) = (Json.parse(blockAt(nodeHttp, blockHeight)) \ "signature").get.as[String]

  def nodeComparator(node1: String, node2: String)(h: Int): Boolean = {
    blockSigAt(node1, h) == blockSigAt(node2, h)
  }

  val TESTNET1 = "http://52.30.47.67:6869"
  val TESTNET2 = "http://52.28.66.217:6869"
  val TESTNET3 = "http://52.77.111.219:6869"
  val TESTNET4 = "http://52.51.92.182:6869"

  val DEVNET1  = "http://34.251.200.245:6869"
  val DEVNET1D = "http://34.251.200.245:16869"
  val DEVNET2  = "http://35.157.212.173:6869"
  val DEVNET2D = "http://35.157.212.173:16869"
  val DEVNET3  = "http://13.229.61.140:6869"
  val DEVNET3D = "http://13.229.61.140:16869"

  val MAINNET1 = "http://138.201.152.163"
  val MAINNET2 = "http://138.201.152.164"
  val MAINNET3 = "http://138.201.152.165" // 626195

  def firstDifferent(min: Int, max: Int, areSame: Int => Boolean): Int = {
    println("searching [" + min + ", " + max + ")")
    if (max - min <= 1) max
    else {
      val split = (min + max) / 2
      if (areSame(split))
        firstDifferent(split, max, areSame)
      else firstDifferent(min, split, areSame)
    }
  }

  println("first different block height is " + firstDifferent(1, 258, nodeComparator(DEVNET3D, DEVNET3)))
}

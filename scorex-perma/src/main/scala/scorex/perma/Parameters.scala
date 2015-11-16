package scorex.perma


object Parameters {

  type DataSegment = Array[Byte]

  //few segments to be stored in a block, so segment size shouldn't be big
  val segmentSize = 1024 //segment size in bytes

  val n = 1024*4 //how many segments in a dataset in total
  val l = 16 //how many segments to store for an each miner

  val k = 4 //number of iterations during scratch-off phase
}

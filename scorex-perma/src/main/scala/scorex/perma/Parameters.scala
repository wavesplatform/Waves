package scorex.perma


object Parameters {

  type DataSegment = Array[Byte]

  val segmentSize = 1024 //segment size in bytes

  val n = 1024*2 //how many segments in a dataset in total
  val l = 1024 //how many segments to store for an each miner

  val k = 4 //number of iterations during scratch-off phase
}

package scorex.network

/*
  def getResponse(message: Message): Try[Message] = {
    require(message.mbId.isDefined)
    val id = message.mbId.get

    //PUT QUEUE INTO MAP SO WE KNOW WE ARE WAITING FOR A RESPONSE
    val blockingQueue = new ArrayBlockingQueue[Message](1)
    messages.put(id, blockingQueue)

    if (!sendMessage(message)) {
      Failure(new Exception("FAILED TO SEND MESSAGE"))
    } else {
      Try {
        val response = blockingQueue.poll(Settings.connectionTimeout, TimeUnit.MILLISECONDS)
        messages.remove(id)
        response
      }
    }
  }

  def sendMessage(message: Message): Unit = {
    self ! ByteString(message.toBytes())
  }

  */
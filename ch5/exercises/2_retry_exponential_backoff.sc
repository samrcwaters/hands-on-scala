val BACKOFF_SECONDS = 2

def logTry(tryNum: Int, maxTries: Int) = {
  s"try ${tryNum}/${maxTries}"
}

// retry with exponential backoff.
// sleep between retries with a configurable initial delay in ms.
def retry[T](max: Int, delay: Int)(f: => T): T = {
  var tries = 0
  var timeOfLastTry = 0L
  var result: Option[T] = None
  while (result == None) {
    try {
      tries match {
        case 0 => println(logTry(tries + 1, max + 1))
        case _ =>
          val backoffMs =
            (scala.math.pow(BACKOFF_SECONDS, tries) * 1000 + delay).toLong
          // println(s"backoff: $backoffMs")
          Thread.sleep(backoffMs)
          val timeOfCurrentTry = System.nanoTime
          println(
            s"${logTry(tries + 1, max + 1)}, ${(timeOfCurrentTry - timeOfLastTry) / 1e6} milliseconds later"
          )
          timeOfLastTry = timeOfCurrentTry
      }
      timeOfLastTry = System.nanoTime
      result = Some(f)
      // backoff before next try
    } catch {
      case e: Throwable =>
        tries += 1
        if (tries > max) throw e
        else {
          println(s"failed, starting retry #$tries")
        }
    }
  }
  result.get
}

val httpbin = "https://httpbin.org"
retry(max = 5, delay = 100) {
  // only succeeds w/ 200 response code 1/3 of the time
  requests.get(s"$httpbin/status/200,400,500")
}

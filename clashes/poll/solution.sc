// by @Andriamanitra 

import scala.collection.mutable._
import scala.io.StdIn._

object Solution extends App {
    var canVote = HashMap[Int, Int]()
    var votes = Array(0, 0)
    val timeout = readLine.toInt
    for(i <- 1 to readLine.toInt) {
        val Array(userId, vote, time) = (readLine split " ") map (_.toInt)
        if (canVote.getOrElse(userId, time) <= time) {
            votes(vote) += 1
            canVote.update(userId, time + timeout)
        }
    }
    println(votes mkString " ")
}
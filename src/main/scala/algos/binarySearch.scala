package algos

import scala.annotation.tailrec

def search(arr: Array[Int], input: Int) = {
  @tailrec
  def bs(l: Int, r: Int): Int = {
    val mid = (l + r) / 2
    if mid <= 0 || mid >= arr.length then -1
    else if arr(mid) == input then mid
    else if arr(mid) > input then bs(l, mid - 1)
    else bs(mid + 1, r)
  }

  bs(0, arr.length)
}

@main def binarySearch(): Unit =
  val input = Array(1, 5, 9, 11)
  println(search(input, 9))

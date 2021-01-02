package algos

def swap(i: Int, j: Int, arr: Array[Int]): Array[Int] = arr.updated(i, arr(j)).updated(j, arr(i)) 

def reorder(index: Int, arr: Array[Int], by: (Int, Int) => Boolean): Array[Int] = {
    val pi = index / 2
    arr match {
        case Array(_)                    => arr
        case _ if by(arr(index), arr(pi)) => reorder(pi, swap(index, pi, arr), by)
        case _                           => arr
    }
}

def insert(elm: Int, arr: Array[Int], by: (Int, Int) => Boolean): Array[Int] = {
    val a = arr :+ elm
    reorder(a.size - 1, a, by)
}

def heap(arr: Array[Int], by: (Int, Int) => Boolean): Array[Int] = {
    def loop(acc: Array[Int], mh: Array[Int]): Array[Int] = 
        acc match {
            case Array() => mh
            case Array(a, rest@_*) => loop(rest.toArray, insert(a, mh, by))
        }

    loop(arr, Array())
}

def maxHeap(arr: Array[Int]): Array[Int] = heap(arr, _ > _)
def minHeap(arr: Array[Int]): Array[Int] = heap(arr, _ < _)


@main
def heap = 
    val input = Array(8, 10, 30, 15, 50, 16, 20)
    val mh = maxHeap(input)
    println("Input    = " + input.mkString(","))
    println("Max Heap = " + mh.mkString(","))
    println("Min Heap = " + minHeap(input).mkString(","))

//            30
//     10          16
// 20      8    15    

//             8
//     10              16
// 15      50      30      20

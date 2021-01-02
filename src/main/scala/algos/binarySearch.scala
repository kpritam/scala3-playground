package algos

def search(arr: Array[Int], input: Int) = {
    def bs(l: Int, r: Int): Int = {
        val mid = (l + r) / 2
        if (mid <= 0 || mid >= arr.length) -1
        else if (arr(mid) == input) mid
        else if (arr(mid) > input) bs(l, mid - 1)
        else bs(mid + 1, r)
    }

    bs(0, arr.length)
}

@main
def binarySearch = 
    val input = Array(1, 5, 9, 11)
    println(search(input, 9))

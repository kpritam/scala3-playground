def swap0(i: Int, j: Int, arr: Array[Int]) = {
    val temp = arr(i) 
    arr(i) = arr(j); 
    arr(j) = temp; 
}

def qs(arr: Array[Int]) = {
    def partition(l: Int, r: Int) = {
        println(arr.mkString(","))
        val p = arr(r)
        var i = l - 1
        
        (l to r).foreach { j =>
            if (arr(j) < p) {
                i += 1
                swap0(i, j, arr)
            }   
        }
        swap0(i + 1, r, arr)
        i + 1
    }

    def qs(l: Int, r: Int): Unit = {
        if(l < r) {
            val p = partition(l, r)
            qs(l, p - 1)
            qs(p + 1, r)
        }
    }

    qs(0, arr.length - 1)

}

@main
def quickSort = 
    val input = Array(5, 10, 2, 17, 13, 3)
    qs(input)
    println(input.mkString(","))
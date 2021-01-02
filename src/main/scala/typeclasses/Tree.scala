package typeclasses

enum Tree[T] derives Eq, Show:
    case  Leaf[T](elm: T) extends Tree[T]
    case  Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

@main def tc() =
    val l1 = Tree.Leaf(1)
    val l2 = Tree.Leaf(2)
    val b1 = Tree.Branch(l1, l2)
    val eq = summon[Eq[Tree[Int]]]
    println(eq.eqv(l1, l2))
    println(eq.eqv(l1, b1))
    println(l1.show)
    println(l2.show)
    println(b1.show)
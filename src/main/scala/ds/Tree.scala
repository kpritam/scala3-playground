package ds

enum Tree[+A]:
  case Leaf(value: A)
  case Node(left: Tree[A], right: Tree[A])

  def size: Int =
    this match
      case Tree.Leaf(_)    => 1
      case Tree.Node(l, r) => 1 + l.size + r.size

  def depth: Int =
    this match
      case Tree.Leaf(_)    => 0
      case Tree.Node(l, r) => 1 + l.depth.max(r.depth)

  def fold[B](leaf: A => B)(node: (B, B) => B): B =
    this match
      case Tree.Leaf(a)    => leaf(a)
      case Tree.Node(l, r) => node(l.fold(leaf)(node), r.fold(leaf)(node))

  def map[B](f: A => B): Tree[B] =
    fold(a => Tree.Leaf(f(a)))(Tree.Node(_, _))

  def sizeViaFold: Int =
    fold(_ => 1)(1 + _ + _)

  def depthViaFold: Int =
    fold(_ => 0)((a, b) => 1 + a max b)

  def foreach(f: A => Unit): Unit =
    map(f)

object TreeApp:
  def run(): Unit =
    println("============ Tree Examples =============")
    val tree =
      Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(2)), Tree.Leaf(3))

    println(s"Tree.size: ${tree.size}")
    println(s"Tree.depth: ${tree.depth}")
    println(s"Tree.sizeViaFold: ${tree.sizeViaFold}")
    println(s"Tree.depthViaFold: ${tree.depthViaFold}")
    print(s"Tree.map: ")
    tree.map(_ * 2).foreach(print)
    println()

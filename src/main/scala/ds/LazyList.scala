package ds

import scala.collection.immutable.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def take(elms: Int): LazyList[A] =
    this match
      case LazyList.Cons(h, t) if elms > 1  => LazyList.cons(h(), t().take(elms - 1))
      case LazyList.Cons(h, _) if elms == 1 => LazyList.cons(h(), LazyList.Empty)
      case _                                => LazyList.Empty

  def map[B](f: A => B): LazyList[B] =
    this match
      case LazyList.Empty      => LazyList.Empty
      case LazyList.Cons(h, t) => LazyList.Cons(() => f(h()), () => t().map(f))

  def foreach(f: A => Unit): Unit =
    map(f)

  def toList: List[A] =
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty      => acc.reverse

    go(this, Nil)

object LazyList:

  def apply[A](xs: A*): LazyList[A] =
    if xs.isEmpty then LazyList.Empty
    else LazyList.Cons(() => xs.head, () => apply(xs.tail: _*))

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def concat[A](l1: LazyList[A], l2: LazyList[A]): LazyList[A] =
    l1 match
      case LazyList.Empty      => l2
      case LazyList.Cons(h, t) => LazyList.Cons(h, () => concat(t(), l2))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

object LazyListApp:
  def run(): Unit =
    println("============ LazyList Examples =============")
    print("LazyList.from(1).take(10): ")
    print(LazyList.from(1).take(10).toList.mkString(","))
    println()

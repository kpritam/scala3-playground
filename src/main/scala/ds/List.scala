package ds

import scala.annotation.tailrec

enum List[+A]:
  case Nil
  case Cons(h: A, t: List[A])

  def foldLeft[B](acc: B, f: (A, B) => B): B =
    @tailrec
    def go(xs: List[A], acc: B, f: (A, B) => B): B =
      xs match
        case List.Nil        => acc
        case List.Cons(h, t) => go(t, f(h, acc), f)

    go(this, acc, f)

  def map[B](f: A => B): List[B] =
    foldLeft[List[B]](List.Nil, (a, acc) => List.Cons(f(a), acc))

  def flatmap[B](f: A => List[B]): List[B] =
    foldLeft[List[B]](List.Nil, (a, acc) => List.concat(acc, f(a)))

  def foreach(f: A => Unit): Unit =
    map(f)

object List:
  def apply[A](xs: A*): List[A] =
    if xs.isEmpty then List.Nil
    else List.Cons(xs.head, apply(xs.tail: _*))

  def concat[A](l1: List[A], l2: List[A]): List[A] =
    l1 match
      case List.Nil        => l2
      case List.Cons(h, t) => List.Cons(h, concat(t, l2))

object ListApp:
  def run(): Unit =
    println("============ List Examples =============")
    print("List.foreach: ")
    List(1, 2, 3, 4).foreach(print)
    println()
    print("List.flatmap: ")
    List(1, 2, 3, 4).flatmap(a => List(a, a * 2)).foreach(print)
    println()
    println("=" * 40)

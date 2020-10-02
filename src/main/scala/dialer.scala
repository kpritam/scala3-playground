val dial = List(
    List(1, 2, 3),
    List(4, 5, 6),
    List(7, 8, 9),
    List(-1, 0, -1)
)

def isValidIndex(x: Int, y: Int): Boolean = (x <= 2 && x >=0 && y <= 2 && y >= 0) || (x == 3 && y == 1)

def findIndex(value: Int) = dial.find(_.contains(value)).map(xs => (dial.indexOf(xs), xs.indexOf(value)))

case class Point private (x: Int, y: Int):
    def next =
        List(
          Point.make(x + 2, y + 1),
          Point.make(x + 2, y - 1),
          Point.make(x + 1, y + 2),
          Point.make(x + 1, y - 2),
          Point.make(x - 2, y - 1),
          Point.make(x - 2, y + 1),
          Point.make(x - 1, y - 2),
          Point.make(x - 1, y + 2)
        ).flatten.distinct

    def sequence = next.flatMap { p1 => p1.next.map(p2 => Sequence(this, p1, p2)) }
    
    def value = dial(x)(y)

object Point:
  def make(x: Int, y: Int): Option[Point] = Option.when(isValidIndex(x, y))(Point(x, y))

case class Sequence(p1: Point, p2: Point, p3: Point):
    override def toString = s"${p1.value}-${p2.value}-${p3.value}"

def findSequence(value: Int) = findIndex(value).flatMap(x => Point.make(x._1, x._2)).map(_.sequence).toList.flatten

@main
def dialer =
  println(findSequence(6).mkString("\n"))

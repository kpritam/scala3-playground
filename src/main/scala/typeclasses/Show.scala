package typeclasses

trait Show[A]:
    extension (a: A) def show: String

object Show:
    import scala.compiletime._
    import scala.deriving.Mirror

    private inline def summonAll[T <: Tuple]: List[Show[_]] = 
        inline erasedValue[T] match 
            case _: EmptyTuple  => Nil
            case _: (t *: ts)   => summonInline[Show[t]] :: summonAll[ts]

    private inline def elemLabels[T <: Tuple]: List[String] = 
        inline erasedValue[T] match 
            case _: EmptyTuple  => Nil
            case _: (t *: ts)   => constValue[t].asInstanceOf[String] :: elemLabels[ts]

    inline def derived[A](using m: Mirror.Of[A]): Show[A] = 
        new Show[A]:
            extension (a: A) def show: String = 
                inline m match 
                    case p: Mirror.ProductOf[A] => showProduct(p, a)
                    case s: Mirror.SumOf[A]     => showSum(s, a)

    inline def showProduct[A](m: Mirror.ProductOf[A], a: A): String = 
        val productName = constValue[m.MirroredLabel]
        val labels = elemLabels[m.MirroredElemLabels]
        val elements = labels.iterator zip a.asInstanceOf[Product].productIterator
        val elemInstances = summonAll[m.MirroredElemTypes]
        val elemStrings = (elements zip elemInstances).map {
        case ((name, value), instance) =>
            s"$name = ${instance.asInstanceOf[Show[Any]].show(value)}"
        }
        s"${productName}(${elemStrings.mkString(", ")})"

    private inline def rec[A, T](n: Int, ord: Int, a: A): String = 
        inline erasedValue[T] match 
            case _: (t *: ts) =>
                if n == ord then summonFrom { case p: Mirror.ProductOf[`t`] => showProduct[t](p, a.asInstanceOf[t]) }
                else rec[A, ts](n + 1, ord, a)
            case _: EmptyTuple => ""

    inline def showSum[A](m: Mirror.SumOf[A], a: A): String = 
        val ord = m.ordinal(a)
        rec[A, m.MirroredElemTypes](0, ord, a)
    

    given Show[Int] with 
        extension (a: Int) def show: String = a.toString

    given Show[String] with 
        extension (a: String) def show: String = s""""$a""""


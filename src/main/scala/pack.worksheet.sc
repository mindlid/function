val listWDuplicated = List("a", "a", "b", "c", "b", "c", "c", "a")
listWDuplicated.filter(i => i.equals("a"))
groupItems("a", listWDuplicated, List())
removeItems("a", listWDuplicated, List())
pack(listWDuplicated, List())

def groupItems[T](item: T, ys: List[T], grouped: List[T]): List[T] =
  ys match {
    case Nil => grouped
    case x :: xs => {
      if x.equals(item) then groupItems(item, xs, x :: grouped)
      else groupItems(item, xs, grouped)
    }
  }

def removeItems[T](item: T, ys: List[T], result: List[T]): List[T] =
  ys match {
    case Nil => result
    case x :: xs => {
      if x.equals(item) then removeItems(item, xs, result)
      else removeItems(item, xs, x :: result)
    }
  }

def pack[T](
    ys: List[T],
    packed: List[List[T]]
): List[List[T]] = ys match {
  case Nil => packed
  case x :: xs => {
    val rem = removeItems(x, xs, List())
    val gro = packed.appended(groupItems(x, xs, List(x)))
    pack(rem, gro)
  }
}

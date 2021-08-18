@main def hello: Unit = {
  val listWDuplicated = List("a", "a", "b", "c", "c", "a")
  listWDuplicated.filter(i => i.equals("a"))
  groupItems("a", listWDuplicated, List())
  removeItems("a", listWDuplicated, List())
  packHelper("a", listWDuplicated, List())
  pack(listWDuplicated)
}

def pack[T](ys: List[T]): List[List[T]] = ys match {
  case Nil     => Nil
  case x :: xs => packHelper(x, xs, List())
}

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

def packHelper[T](
    item: T,
    ys: List[T],
    packed: List[List[T]]
): List[List[T]] = ys match {
  case Nil => packed
  case x :: xs => {
    val rem = removeItems(item, ys, List())
    val gro = packed.appended(groupItems(item, ys, List()))
    packHelper(x, rem, gro)
  }
}

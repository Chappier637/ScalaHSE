object SolutionHW2 {
  def MySum(a: List[Int]): Int = {
    a.foldLeft(0)((acc, t) => acc + t)
  }

  def MyRange(n: Int, m: Int): List[Int] = {
    (n, m) match {
      case (n, m) if n > m => Nil
      case (n, m) if n == m => List(n)
      case (n, m) => n :: MyRange(n+1, m)
    }
  }

  def main(args: Array[String]): Unit = {
    println(MySum(MyRange(1, 5)))
    println(MySum(MyRange(1, 1)))
    println(MySum(MyRange(5, 1)))
  }
}

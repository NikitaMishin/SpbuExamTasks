
package solver

import breeze.linalg._

import scala.annotation.tailrec

/*
  Matrix n x 4
  where 3 coefficient and f value


 */
object Main extends App {

  val C = 0
  val B = 1
  val A = 2

  val inp = input()

  println(solver(inp._1, inp._2, Seq.empty))

  @tailrec def solver(curA: DenseMatrix[BigDecimal], curF: DenseVector[BigDecimal], acc: Seq[(DenseMatrix[BigDecimal], DenseVector[BigDecimal])], k: Int = 1): DenseVector[BigDecimal] = {

    if (curA.rows == 1) {
      //backward
      var vectorX = DenseVector(Array(curF(0) / curA(0, B))) // x1 = f1/b1
      //на каждом шаге вычисляем соответствующий вектор X  для матрицы Ak и Fk
      //в самом конце получаем решение исходной матрицы
      for (l <- 2 to k) {
        val (matrix: DenseMatrix[BigDecimal], vectorF: DenseVector[BigDecimal]) = acc(l - 2)
        val prevX = vectorX.copy

        vectorX = DenseVector.zeros(matrix.rows)
        // вычисляем значение xi c четными индексами(засчет индексации с нуля у нас наоборот)
        for (i <- 1 until matrix.rows by 2)
          vectorX(i) = prevX(i / 2) // x(k)i = x(k+1)i/2

        // вычисляем значения xi с нечет индексами
        List.range(0, matrix.rows, 2).par.foreach {
          case 0 => // x1 = f1 /b1 -(a1/b1)*x2
            vectorX(0) = (vectorF(0) - matrix(0, A) * vectorX(1)) / matrix(0, B)
          case i@(n: Int) if n == matrix.rows - 1 => // xn = fn/bn -(cn/b1)*x(n-1)
            vectorX(i) = (vectorF(i) - matrix(i, C) * vectorX(i - 1)) / matrix(i, B)
          case i => //xi = fi/bi - (ai/bi)/x(i+1) - (ci/bi)*x(i-1)
            vectorX(i) = (vectorF(i) - matrix(i, C) * vectorX(i - 1) - matrix(i, A) * vectorX(i + 1)) / matrix(i, B)
        }
      }

      return vectorX
    }

    //forward move

    val currentSize = curA.rows
    val nextSize = (curA.rows - 1) / 2
    val nextA = DenseMatrix.zeros[BigDecimal](nextSize, 3)
    val nextF = DenseVector.zeros[BigDecimal](nextSize)
    List.range(1, currentSize, 2).par.foreach(i => {

      /*вычисление коэфф для k+1 матрицы через k
        c(i/2)= −ci * c(i−1)/b(i−1),
      a(i/2)= −ai * a(i+1)/b(i+1),
      b(i/2)= bi − ci* a(i−1)/b(i−1) − ai*c(i+1)/b(i+1),
      f(i/2)= fi − ci* f(i−1)/b(i−1) − ai*f(i−1)/b(i−1)
      */

      val alpha = -curA(i, C) / curA(i - 1, B)
      val beta = -curA(i, A) / curA(i + 1, B)
      nextA(i / 2, ::) := DenseVector(Array(
        alpha * curA(i - 1, C),
        curA(i, B) + alpha * curA(i - 1, A) + beta * curA(i + 1, C),
        beta * curA(i + 1, A)
      )).t
      nextF(i / 2) = curF(i) + alpha * curF(i - 1) + beta * curF(i + 1)
    }
    )

    solver(nextA, nextF, (curA, curF) +: acc, k + 1)
  }

  // def addFakeEquations(matrix: DenseMatrix[BigDecimal], vectorF: DenseVector[BigDecimal]): (DenseMatrix[BigDecimal], DenseVector[BigDecimal], Int) = {
  //  if ((matrix.rows + 1) & matrix.rows == 0)
  //    (matrix, vectorF, matrix.rows) // ok 2^q - 1
  //else {
  //  ???
  // }
  //}

  /*
  Чтение пользовательского ввода
  */
  def input(): (DenseMatrix[BigDecimal], DenseVector[BigDecimal]) = {
    println("Введите кол-во уравнений:")
    val n = scala.io.StdIn.readLine().toInt
    val b = DenseVector.zeros[BigDecimal](n)
    val A = DenseMatrix.zeros[BigDecimal](n, 3)
    println("Введите ур-я:")
    for (i <- 1 to n) {
      val input = scala.io.StdIn.readLine().split(" ").toSeq.map(x => BigDecimal(x.toDouble))
      b(i - 1) = input.last
      i match {
        case 1 =>
          A(i - 1, ::) := DenseVector((BigDecimal(0.0) +: input.take(2)).toArray).t
        case number if number == n =>
          A(i - 1, ::) := DenseVector((input.take(2) :+ BigDecimal(0.0)).toArray).t
        case _ =>
          A(i - 1, ::) := DenseVector(input.take(3).toArray).t
      }
    }
    (A, b)
  }

  /*
      Вывод вектора на экран
   */
  def output(denseVector: DenseVector[BigDecimal]): Unit = println(denseVector)
}

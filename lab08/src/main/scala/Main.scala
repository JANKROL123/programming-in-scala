def masterMind[A](secret:List[A], guess:List[A]): (Int, Int) = {
  val black = secret.zip(guess).count(n => n match {case (a,b) => a == b })
  val white = guess.toSet.toList.map(n => secret.count(m => m == n) min guess.count(m => m == n)).sum - black 
  (black, white)
}
val zawodnicy = List(
  "Piotr Nowak",
  "Jan Kowalski",
  "Krzysztof Wiśniewski",
  "Marek Wójcik",
  "Mirosław Kowalczyk",
  "Mateusz Kamiński",
  "Paweł Lewandowski",
  "Maciej Zieliński",
  "Wojciech Szymański",
  "Mariusz Woźniak",
  "Antoni Dąbrowski",
  "Jakub Kozłowski",
  "Szymon Jankowski",
  "Stanisław Mazur",
  "Aleksander Wojciechowski",
  "Konrad Kwiatkowski",
  "Filip Krawczyk",
  "Marian Kaczmarek",
  "Leszek Piotrowski",
  "Patryk Grabowski"
)
val r = util.Random
val rating = zawodnicy.flatMap(n => {
  (0 until 20).toList.map(item => {
    val x = r.nextInt(20)
    val y = r.nextInt(20)
    n.split(" ").toList match {case List(a,b) => (a,b,x,y)}
  })
}).groupMap(n => (n._1, n._2))(n => (n._3, n._4)).mapValues(n => {
  val folded = n.foldLeft((0.0,0.0))((a,b) => (a,b) match {case ((a1,a2), (b1,b2)) => (a1+b1, a2+b2)})
  folded match {case (a,b) => (a+b)/(n.length.toDouble)}
}).toList.map(n => n match {case ((a,b),c) => (a,b,c)}).sortBy(n => (-n._3, n._2))
.zipWithIndex.map(n => n match {case ((a,b,c),idx) => (idx+1, a, b, c)})
def threeNumbers(n:Int): List[(Int,Int,Int)] = {
  val list = (1 to n).toList
  for {
    a <- list
    b <- list 
    c <- list
    if a < b && a*a + b*b == c*c 
  } yield (a,b,c)
}
@main
def zadanie_30: Unit = {
  val secret = List(1, 3, 2, 2, 4, 5)
  val guess  = List(2, 1, 2, 4, 7, 2)
  val secret2 = List(2, 5, 9, 4, 2, 3, 4, 5, 1)
  val guess2  = List(4, 5, 7, 2, 8, 3, 9, 5, 4)
  println(masterMind(secret, guess))
  println(masterMind(secret2, guess2))
}
@main
def zadanie_31: Unit = {
  rating.foreach(n => n match {case (idx,fName,lName,score) => println(s"${idx}. $fName $lName - $score")})
}
@main
def zadanie_32: Unit = {
  println(threeNumbers(15))
}
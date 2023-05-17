def parzysta(n:Int): Boolean = {
    n % 2 == 0
}
def nwd(a:Int, b:Int): Int = {
    if (a == b) a 
    else {
        if (a > b) nwd(a-b,b)
        else nwd(a,b-a)
    }
}
def assert(n:Int): Boolean = {
    n >= 2
}
def pierwsza(n:Int): Boolean = {
    def recPierwsza(n:Int, acum:Int): Boolean = {
        if (acum == n) true
        else {
            if (n % acum == 0) false
            else recPierwsza(n, acum+1) 
        }
    }
    if (assert(n)) recPierwsza(n, 2)
    else false
}
def hipoteza(n:Int): Unit = {
    def recHipoteza(n:Int, other:Int): (Int, Int) = {
        if (pierwsza(n-other) && pierwsza(other)) (n-other,other)
        else recHipoteza(n, other+1)
    }
    println(recHipoteza(n, 2))
}




@main
def zadanie_02: Unit = {
    val n = 5
    if (parzysta(n)) println("tak")
    else println("nie")
}
@main
def zadanie_03: Unit = {
    println(s"nwd(100,80) == ${nwd(100,80)}")
    println(s"nwd(5,6) == ${nwd(5,6)}")
    println(s"nwd(36,45) == ${nwd(36,45)}")
}
@main
def zadanie_04(liczba:Int): Unit = {
    println(s"pierwsza($liczba) == ${pierwsza(liczba)}")
}
@main
def zadanie_05(n:Int): Unit = {
    if (n % 2 == 0 && n >= 2) hipoteza(n)
    else println("Nieodpowiednia liczba")
}
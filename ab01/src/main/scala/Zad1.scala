@main 
def ćwiczenie01: Unit = {
    val napis = """Ala ma
                    |kota i psa""".stripMargin
    val list = napis.split("\r\n").toList
    val len = list.maxBy(n => n.length).length
    var result = ""
    if (len % 2 == 0) {
        result += "* "*((len+6)/2)+"\n"
    } else {
        result += "* "*((len+5)/2)+"\n"
    }
    for (i <- list) {
        if (len % 2 == 0) {
            result += "* "+i+" "*(len-i.length+2)+"*\n"
        } else {
            result += "* "+i+" "*(len-i.length+1)+"*\n"
        }
    }
    if (len % 2 == 0) {
        result += "* "*((len+6)/2)
    } else {
        result += "* "*((len+5)/2)
    }
    println(result)
}
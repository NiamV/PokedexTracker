class Pokemon(id: Int, name: String, imgPath: String, inList: Boolean, caught: Boolean, order: Int){
    def id(): Int = id
    def name(): String = name
    def imgPath(): String = imgPath

    var isCaught: Boolean = caught

    def caught(): Boolean = isCaught
    def updateCaught(): Unit = {
        isCaught = !isCaught
    }

    var isInList: Boolean = inList

    def inList(): Boolean = isInList
    def updateInList(): Unit = {
        isInList = !isInList
    }

    var order_var = order

    def order(): Int = order_var
    def changeOrder(newOrder: Int): Unit = {
        order_var = newOrder
    }

    def printPokemon(): String = {
        def boolString(b: Boolean): String = {
            if(b){
                return "TRUE"
            } 
            return "FALSE"
        }

        val output = id.toString + "," + name + "," + inList.toString + "," + caught.toString
        return output
    }
}

class User(file: String){
    def file(): String = file
    
    def getData(file: String): Array[Pokemon] = {
        val data = scala.io.Source.fromFile(file).getLines.toArray.tail
        val pokemon = new Array[Pokemon](data.length)

        def parseInputLine(s: String): Pokemon = {
            val line = s.split(",")
            return new Pokemon(line(0).toInt, line(1), line(2), line(3).toBoolean, line(4).toBoolean, line(5).toInt)
        }

        var i = 0
        while(i < pokemon.length){
            pokemon(i) = parseInputLine(data(i))
            i += 1
        }

        return pokemon
    }

    val pokes = getData(file)

    def getPokes(): Array[Pokemon] = pokes

    def getById(start: Int, end: Int): Array[Pokemon] = {
        return pokes.filter(_.id() >= start).filter(_.id() <= end)
    }

    def percentCaught(): Double = {
        val inList = pokes.filter(_.inList).length.toDouble
        val caught = pokes.filter(_.inList).filter(_.caught).length.toDouble
        return 100 * caught / inList
    }

    def findPokemon(id: Int): Int = {
        var index = 0
        while(index < pokes.length && pokes(index).id != id){
            index += 1
        }
        return index
    }

    def findPokemonByName(name: String): Int = {
        var index = 0
        while(pokes(index).name != name){
            index += 1
        }
        return index
    }

    var maxInList = pokes.filter(_.order() < 10000).sortBy(_.order()).reverse.head.order()
    var maxNotInList = pokes.filter(_.order() > 10000).sortBy(_.order()).reverse.head.order()

    def removeFromOrder(id: Int): Unit = {
        val index = findPokemon(id)

        val orderPos = pokes(index).order()

        if(orderPos < 10000){
            for(p <- pokes){
                val po = p.order()
                if(po > orderPos && po < 10000){
                    p.changeOrder(po - 1)
                }
            }

            pokes(index).changeOrder(maxNotInList + 1)
            maxNotInList += 1
        }
    }

    def addToOrder(id: Int): Unit = {
        val index = findPokemon(id)

        if(pokes(index).order() >= 10000){
            pokes(index).changeOrder(maxInList + 1)
            maxInList += 1
        }
    }

    def printPokes(): String = {
        var output = ""

        var i = 0
        var j = 10000

        for(p <- pokes){
            output += p.id.toString
            output += ","
            output += p.name.toString
            output += ","
            output += p.imgPath.toString
            output += ","
            output += p.inList.toString
            output += ","
            output += p.caught.toString
            output += ","
            output += p.order.toString
            output += "\n"
        }
        return output
    }

    def updateCSV(): Unit = {
        import java.io.File
        import java.io.PrintWriter

        val writer = new PrintWriter(new File(file))

        writer.write("ID,Name,ImgPath,InList,Caught,Order\n")
        writer.write(printPokes())
        writer.close()
    }
}
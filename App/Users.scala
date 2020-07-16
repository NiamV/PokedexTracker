class Pokemon(id: Int, name: String, imgPath: String, inList: Boolean, caught: Boolean){
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
            return new Pokemon(line(0).toInt, line(1), line(2), line(3).toBoolean, line(4).toBoolean)
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

    def printPokes(): String = {
        var output = ""
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
            output += "\n"
        }
        return output
    }

    def updateCSV(): Unit = {
        import java.io.File
        import java.io.PrintWriter

        val writer = new PrintWriter(new File(file))

        writer.write("ID,Name,InList,Caught\n")
        writer.write(printPokes())
        writer.close()
    }
}
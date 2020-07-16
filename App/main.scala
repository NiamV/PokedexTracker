import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import java.awt.Color
 
object Pokedex extends SimpleSwingApplication{
    // Functions to display boxes

    def getCaughtColour(caught: Boolean): Color = {
        if(caught){
            return new Color(184, 242, 150)
        }
        return new Color(255, 184, 166)
    }

    def getinListColour(inList: Boolean): Color = {
        if(inList){
            return new Color(255, 255, 255)
        }
        return new Color(100, 100, 100)
    }

    def makeImg(u: User, id: Int): BoxPanel = {
        val p = new BoxPanel(Orientation.Horizontal){
            //println(id)
            val index = u.findPokemon(id)

            var currentName = ""                        
            if(id != 0){
                currentName = u.pokes(index).imgPath().toLowerCase
            } else {
                currentName = "unknown"
            }   

            val path = s"regular/$currentName.png"
            
            if(id == 0){
                val label = new Label {
                    val img = new ImageIcon(path)
                    val image = img.getImage(); // transform it 
                    val newimg = image.getScaledInstance(75, 75,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  

                    icon = new ImageIcon(newimg);  // transform it back
                }

                contents += label
            } else {
                val label = new Button {
                    val img = new ImageIcon(path)
                    val image = img.getImage(); // transform it 
                    val newimg = image.getScaledInstance(75, 60,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  
                    
                    icon = new ImageIcon(newimg);  // transform it back

                    background = getCaughtColour(u.pokes(index).caught)
                    
                    tooltip = u.pokes(index).name()
                }

                listenTo(label)
                reactions += {
                    case ButtonClicked(b) =>
                        u.pokes(index).updateCaught()
                        b.background = getCaughtColour(u.pokes(index).caught)
                        u.updateCSV()

                        percent = u.percentCaught()
                        percentProgress.value = percent.toInt
                        percentProgress.label = percent.toString.take(5) + "%"
                }

                contents += label
            }

            val d = new Dimension(50, 50)
            maximumSize = d
            minimumSize = d
            preferredSize = d
        }

        return p
    }
    
    def makeBox(u: User, pokemon: Array[Pokemon], boxNo: Int): BorderPanel = {
        assert(pokemon.length <= 30)
        val box = new GridPanel(5,6){
            val d = new Dimension(670,400)

            maximumSize = d
            minimumSize = d
            preferredSize = d
        }

        var i = 0
        while(i < 30 && i < pokemon.length){
            val id = pokemon(i).id()
            box.contents += makeImg(u, id)
            i += 1
        }

        while(i < 30){
            box.contents += makeImg(u, 0)
            i += 1
        }

        val boxHeader = new BorderPanel{
            val label = new Label(s"Box $boxNo")
            label.font = new java.awt.Font("Handwriting - Dakota", java.awt.Font.PLAIN, 30)

            add(label, BorderPanel.Position.Center)

            val allButton = new Button{
                text = "Select All"
            }

            val notAllButton = new Button{
                text = "Deselect All"
            }

            listenTo(allButton, notAllButton)
            reactions += {
                case ButtonClicked(b) => {
                    for(p <- pokemon){
                        val index = u.findPokemon(p.id())
                        u.pokes(index).isCaught = (b.text == "Select All")
                    }

                    u.updateCSV()

                    percent = u.percentCaught()
                    percentProgress.value = percent.toInt
                    percentProgress.label = percent.toString.take(5) + "%"
                    Swing.onEDT(render(u))
                }
            }

            val allButtons = new BoxPanel(Orientation.Horizontal){
                contents += allButton
                contents += notAllButton
            }
            add(allButtons, BorderPanel.Position.East)
        }

        val borderBox = new BorderPanel{
            add(boxHeader, BorderPanel.Position.North)
            add(box, BorderPanel.Position.Center)
            add(new Label(" "), BorderPanel.Position.South)

            val d = new Dimension(670, 400)

            maximumSize = d
            minimumSize = d
            preferredSize = d
        }

        return borderBox
    }

    def render(u: User): Unit = {
        percent = u.percentCaught()
        percentProgress.value = percent.toInt
        percentProgress.label = percent.toString.take(5) + "%"

        mainBody.columnHeaderView = new BoxPanel(Orientation.Vertical){
            contents += new FlowPanel{
                contents += new Label("Caught: ")
                contents += percentProgress
            }

            contents += new FlowPanel{
                contents += changeSourceFile
                contents += editListButton
            }
        }

        listenTo(changeSourceFile)
        listenTo(editListButton)

        reactions += {
            case ButtonClicked(b) => {
                if(b == changeSourceFile){
                    var fileChoose = new FileChooser(null)
                    val result = fileChoose.showOpenDialog(null)

                    if (result == FileChooser.Result.Approve) {
                        val user = new User(fileChoose.selectedFile.getPath())
                        render(user)
                    }
                }

                else if(b == editListButton){
                    renderEditor(u)                       
                }
            }
        }

        mainBody.viewportView = new BoxPanel(Orientation.Vertical){
            var remainingPokemon = u.getPokes.filter(_.inList())

            var boxNo = 1                
            while(!remainingPokemon.isEmpty){
                val currentBox = makeBox(u, remainingPokemon.take(30), boxNo)
                contents += currentBox

                remainingPokemon = remainingPokemon.drop(30)
                boxNo += 1
            }
        }
    }

    def renderEditor(u: User): Unit = {
        def makePokemonChooser(p: Pokemon): BoxPanel = {
            val cell = new BoxPanel(Orientation.Horizontal){
                val index = u.findPokemon(p.id())
                var currentName = u.pokes(index).imgPath().toLowerCase
                val path = s"regular/$currentName.png"
                
                val img = new Button {
                    val img = new ImageIcon(path)
                    val image = img.getImage(); // transform it 
                    val newimg = image.getScaledInstance(30, 30,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  
                    
                    icon = new ImageIcon(newimg);  // transform it back

                    background = getinListColour(u.pokes(index).inList)
                    
                    tooltip = u.pokes(index).name()

                    val d = new Dimension(50, 30)
                    maximumSize = d
                    minimumSize = d
                    preferredSize = d
                }

                listenTo(img)
                reactions += {
                    case ButtonClicked(b) =>
                        u.pokes(index).updateInList()
                        b.background = getinListColour(u.pokes(index).inList)
                }

                if(p.id() > 4000 && p.id() < 7000){
                    contents += new Label("       ")
                }
                contents += img
                contents += new Label(u.pokes(index).name())

                val d = new Dimension(200, 30)
                maximumSize = d
                minimumSize = d
                preferredSize = d
            }

            return cell
        }
        
        def makeList(column: Array[Pokemon], title: String): BorderPanel = {
            val l = new BoxPanel(Orientation.Vertical){
                val listTitle = new Label(title)
                listTitle.font = new java.awt.Font("Handwriting - Dakota", java.awt.Font.PLAIN, 24)

                contents += new BorderPanel{
                    add(listTitle, BorderPanel.Position.West)
                }

                val buttonSize = new Dimension(130,20)

                val allButton = new Button{
                    text = "Select All"
                    preferredSize = buttonSize
                }

                val allFormsButton = new Button{
                    text = "Select All Forms"
                    preferredSize = buttonSize
                }

                val notAllButton = new Button{
                    text = "Deselect All"
                    preferredSize = buttonSize
                }

                listenTo(allButton, notAllButton, allFormsButton)
                reactions += {
                    case ButtonClicked(b) => {
                        for(p <- column){
                            val index = u.findPokemon(p.id())

                            if(p.id() < 1000 || p.id() > 7000){
                                u.pokes(index).isInList = (b.text == "Select All" || b.text == "Select All Forms")
                            } else {
                                u.pokes(index).isInList = (b.text == "Select All Forms")
                            }
                        }

                        u.updateCSV()
                        renderEditor(u)
                    }
                }
                
                contents += new BorderPanel{
                    add(allButton, BorderPanel.Position.East)
                }

                contents += new BorderPanel{
                    add(allFormsButton, BorderPanel.Position.East)
                }

                contents += new BorderPanel{
                    add(notAllButton, BorderPanel.Position.East)
                }
                
                for(p <- column){
                    contents += makePokemonChooser(p)
                }
            }

            val borderL = new BorderPanel{
                add(l, BorderPanel.Position.North)
            }

            return borderL
        }

        mainBody.columnHeaderView = new BoxPanel(Orientation.Vertical){
            contents += new FlowPanel{
                contents += new Button(
                    Action("Done"){
                        u.updateCSV()

                        percent = u.percentCaught()
                        percentProgress.value = percent.toInt
                        percentProgress.label = percent.toString.take(5) + "%"

                        render(u)
                    }
                )
            }
        }

        mainBody.viewportView = new BoxPanel(Orientation.Vertical){
            val lists = new BoxPanel(Orientation.Horizontal){
                val masterList = u.getPokes()
                val mainList = masterList.filter(_.id() < 7000)

                val gen1 = mainList.takeWhile(_.id() != 152)
                val gen2 = mainList.dropWhile(_.id() != 152).takeWhile(_.id != 252)
                val gen3 = mainList.dropWhile(_.id() != 252).takeWhile(_.id != 387)
                val gen4 = mainList.dropWhile(_.id() != 387).takeWhile(_.id != 494)
                val gen5 = mainList.dropWhile(_.id() != 494).takeWhile(_.id != 650)
                val gen6 = mainList.dropWhile(_.id() != 650).takeWhile(_.id != 722)
                val gen7 = mainList.dropWhile(_.id() != 722).takeWhile(_.id != 810)
                val gen8 = mainList.dropWhile(_.id() != 810)

                val mega = u.getById(7000,7499)
                val gmax = u.getById(7500, 7999)
                val alolan = u.getById(8000, 8499)
                val galarian = u.getById(8500, 8999)
                val alcremie = u.getById(9000, 9199)
                val arceus = u.getById(9200, 9299)
                val silvally = u.getById(9300, 9399)
                val unown = u.getById(9400, 9499)
                val vivillon = u.getById(9500, 9599)

                contents += makeList(gen1, "Gen 1")
                contents += makeList(gen2, "Gen 2")
                contents += makeList(gen3, "Gen 3")
                contents += makeList(gen4, "Gen 4")
                contents += makeList(gen5, "Gen 5")
                contents += makeList(gen6, "Gen 6")
                contents += makeList(gen7, "Gen 7")
                contents += makeList(gen8, "Gen 8")

                contents += makeList(mega, "Mega Evolutions")
                contents += makeList(gmax, "Gigantamax")
                contents += makeList(alolan, "Alolan Forms")
                contents += makeList(galarian, "Galarian Forms")
                contents += makeList(alcremie, "Alcremie Forms")
                contents += makeList(arceus, "Arceus Forms")
                contents += makeList(silvally, "Silvally Forms")
                contents += makeList(unown, "Unown Letters")
                contents += makeList(vivillon, "Vivillon Patterns")
            }

            contents += lists
        }
    }

    // Global Components

    var percent: Double = 0.0 
    val percentProgress = new ProgressBar{
        min = 0
        max = 100
        value = percent.toInt

        label = percent.toString.take(5) + "%"
        labelPainted = true

        foreground = new Color(33, 128, 9)
        background = new Color(245, 118, 118)

        val d = new Dimension(300, 30)
        maximumSize = d
        minimumSize = d
        preferredSize = d
    }

    var head = new FlowPanel{
        contents += new Label("Caught: ")
        contents += percentProgress
    }

    val mainBody = new ScrollPane{
        columnHeaderView = new BoxPanel(Orientation.Vertical)

        rowHeaderView = new BorderPanel{
            val notesLabel = new Label("Notes:")
            notesLabel.font = new java.awt.Font("Handwriting - Dakota", java.awt.Font.PLAIN, 24)

            val notesLabelPanel = new BorderPanel{
                add(notesLabel, BorderPanel.Position.West)
            }

            add(notesLabelPanel, BorderPanel.Position.North)

            val notes = new TextArea(""){
                lineWrap = true
                preferredSize = new Dimension(300, 300)
            }
            notes.font = new java.awt.Font("Handwriting - Dakota", java.awt.Font.PLAIN, 16)

            add(notes, BorderPanel.Position.Center)
        }

        viewportView = new BoxPanel(Orientation.Vertical)
    }

    val changeSourceFile = new Button{
        text = "Change User"
    }

    val editListButton = new Button{
        text = "Edit List"
    }

    // Main function    

    override def top = new MainFrame {
        //val me = new User("data.csv")

        var me: User = null
        val openingChoice = Dialog.showOptions(
            message = "What would you like to do?",
            optionType = Dialog.Options.YesNo,
            entries = List("Create new User", "Load existing user"),
            initial = 0
        )

        if(openingChoice == Dialog.Result.Yes){
            val r = Dialog.showInput(null, "Username:", initial="")

            if(r == None){
                quit()
            }
            else{
                val path = "UserList/" + r.get + ".csv"

                if(scala.reflect.io.File(path).exists){
                    Dialog.showMessage(message = "User already exists, please try again")
                    quit()
                } else {
                    val file = new java.io.File(path)
                    val bw = new java.io.BufferedWriter(new java.io.FileWriter(file))

                    val toCopyText = scala.io.Source.fromFile("basicUser.csv")
                    bw.write(toCopyText.getLines.mkString("\n"))

                    toCopyText.close()
                    bw.close()
                    
                    me = new User(path)
                }
            }

        }
        else if(openingChoice == Dialog.Result.No){
            Dialog.showMessage(null, "Please choose a csv file which contains your data", title=" ")

            var fileChoose = new FileChooser(new java.io.File("./UserList"))
            val result = fileChoose.showOpenDialog(null)

            
            if (result == FileChooser.Result.Approve) {
                me = new User(fileChoose.selectedFile.getPath())
            }else{
                quit()
            }
        } 
        else{
            quit()
        }

        render(me)
        
        contents = new BoxPanel(Orientation.Vertical){
            contents += mainBody
        }
        
        size = new Dimension(1000,1000)
        title = "Pokedex Tracker"
        this.maximize()
    }
}

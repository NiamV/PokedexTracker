import scala.swing._
import scala.swing.event._

class Editor(u: User) extends SimpleSwingApplication{
    override def top = new MainFrame {frame =>
        contents = new BoxPanel(Orientation.Vertical){
            contents += new Label("Hi!")
            val exitBtn = new Button(
                Action("Exit"){
                    u.pokes(0).updateCaught()
                    Pokedex.render(u)
                    
                    close()
                }
            )
            contents += exitBtn
        }
    }
}

//Dialog.showMessage(contents.head, "hi", title="You pressed me")
//new Editor(me).top.visible = true
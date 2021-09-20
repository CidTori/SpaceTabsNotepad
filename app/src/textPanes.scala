import java.awt.Event.{CTRL_MASK, SHIFT_MASK}
import java.awt.event.KeyEvent.VK_Z
import java.awt.{Canvas, Color, Font, FontMetrics}
import java.nio.file.Path
import javax.swing.{JEditorPane, JFrame, KeyStroke, SwingUtilities}
import scala.swing.Dialog.Result
import javax.swing.event.{DocumentEvent, UndoableEditEvent}
import javax.swing.text._
import javax.swing.text.DocumentFilter.FilterBypass
import javax.swing.undo.{CannotRedoException, CannotUndoException, UndoManager}
import scala.swing.{Action, Dialog, TextPane}
import scala.swing.event.{Key, KeyPressed}
import BuildInfo.{appName, appVersion}
import elasticTabstops.{calcElasticTabstopPositions, spacesToSpaceTabs, spaceTabsToSpaces, split, splitAndStrip}
import fileHandling.{chooseAndLoadTextFile, loadScratchFile, loadTextFile, saveTextFile, saveTextFileAs, scratchFilePath}
import settings.{FontCC, Settings}


package object textPanes {

  class ElasticTextPane(var elasticFont: Font, var emptyColumnWidth: Double, var columnPadding: Double) extends TextPane {

    var fm = new Canvas().getFontMetrics(elasticFont)
    setFont(elasticFont)
    setElasticTabstopsDocFilter()

    def getPxVersionsOfEmSettings(): (Int, Int) = {
      val fontMetrics = new Canvas().getFontMetrics(elasticFont)
      val em = fontMetrics.getHeight  // more accurate than the font's point size (with Merriweather at least)
      val emptyColumnWidthPx = (emptyColumnWidth * em).toInt
      val columnPaddingPx = (columnPadding * em).toInt
      val emptyColumnWidthMinusPaddingPx = emptyColumnWidthPx - columnPaddingPx
      (emptyColumnWidthMinusPaddingPx, columnPaddingPx)
    }

    var (emptyColumnWidthMinusPaddingPx, columnPaddingPx) = getPxVersionsOfEmSettings

    def changeSettings(newElasticFont: Font, newEmptyColumnWidth: Double, newColumnPadding: Double): Unit = {
      elasticFont = newElasticFont
      emptyColumnWidth = newEmptyColumnWidth
      columnPadding = newColumnPadding

      val pxSettings = getPxVersionsOfEmSettings
      emptyColumnWidthMinusPaddingPx = pxSettings._1
      columnPaddingPx = pxSettings._2

      fm = new Canvas().getFontMetrics(elasticFont)
      setFont(elasticFont)
      setElasticTabstopsDocFilter()
      alignTabstops()  // force update of tabstop positions
    }

    protected def setFont(font: Font) = {
      val attributes = new SimpleAttributeSet
      StyleConstants.setBackground(attributes, background)
      StyleConstants.setForeground(attributes, foreground)
      StyleConstants.setFontFamily(attributes, font.getFamily)
      StyleConstants.setFontSize(attributes, font.getSize)
      val doc = peer.getDocument.asInstanceOf[StyledDocument]
      doc.setParagraphAttributes(0, doc.getLength, attributes, true)
    }

    def onChange(): Unit = {}

    def getCaretsLineNumAndPos(): (Int, Int) = {
      val caretPos = peer.getCaretPosition
      val root = peer.getDocument.getDefaultRootElement
      val lineNum = root.getElementIndex(caretPos)
      val startOfLineOffset = root.getElement(lineNum).getStartOffset
      val posOnLine = caretPos - startOfLineOffset
      val lineTextToCaret = this.text.drop(startOfLineOffset).take(posOnLine)
      val minimalWhitespacePos = lineTextToCaret.replaceAll(" \t|  +", " ").length
      (lineNum, minimalWhitespacePos)
    }

    @annotation.tailrec
    final def minimiseAlignmentWhitespace(unprocessed: List[(Char, Int)],
                                          processed: List[(Char, Int)] = Nil): List[(Char, Int)] =
      unprocessed.headOption match {
        case None => processed
        case Some(charAndPos) => {
          val (newCharAndPos, dropLength) = charAndPos match {
            case (char, pos) if char == ' ' => {
              val spaceAndPos = (' ', pos)
              val run = unprocessed.takeWhile { case (c, _) => c == ' ' }
              if (run.length > 1 || unprocessed(1)._1 != '\t') (spaceAndPos, run.length)
              else (spaceAndPos, 2)
            }
            case nonSpaceAndPos => (nonSpaceAndPos, 1)
          }
          minimiseAlignmentWhitespace(unprocessed.drop(dropLength), processed :+ newCharAndPos)
        }
      }

    def setCaretsLineNumAndPos(lineNumAndPos: (Int, Int)): Unit = {
      val (lineNum, minimalWhitespacePos) = lineNumAndPos
      val root = peer.getDocument.getDefaultRootElement
      val startOfLineOffset = root.getElement(lineNum).getStartOffset
      val indexedLineText = split(this.text, '\n').drop(lineNum).take(1).flatten.zipWithIndex
      val minimalWhitespaceOnly = minimiseAlignmentWhitespace(indexedLineText.toList)
      val pos = minimalWhitespaceOnly.lift(minimalWhitespacePos) match {
        case Some((_, pos)) => pos
        case None => minimalWhitespaceOnly.lastOption match {
          case Some((_, pos)) => pos + 1
          case None => 0
        }
      }
      peer.setCaretPosition(startOfLineOffset + pos)
    }

    def setNewText(text: String): Unit = {
      this.text = text
      peer.setCaretPosition(0)
      peer.grabFocus
      undoManager.discardAllEdits
    }

    def updateText(text: String): Unit = {
      val caretsLineNumAndPos = getCaretsLineNumAndPos()
      this.text = text
      setCaretsLineNumAndPos(caretsLineNumAndPos)
      peer.grabFocus
      // TODO: At the moment we discard the undo history when we update text, because converting from elastic to
      // non-elastic and vice versa are currently not undoable events, but it would be nice to be able to undo them.
      undoManager.discardAllEdits
    }

    private def getRecalcRange(textPerLine: List[String], startLineNum: Int, nofLines: Int): (Int, Int) = {
      val indexedLines = textPerLine.zipWithIndex
      val recalcStart = indexedLines.take(startLineNum).reverse.find(_._1.count(_ == '\t') == 0) match {
        case None => 0
        case Some((_, lineNum)) => lineNum
      }
      val recalcEnd = indexedLines.drop(startLineNum + nofLines).find(_._1.count(_ == '\t') == 0) match {
        case None => indexedLines.length
        case Some((_, lineNum)) => lineNum + 1
      }
      val recalcLength = recalcEnd - recalcStart
      (recalcStart, recalcLength)
    }

    @annotation.tailrec
    final def countTabstopsPerCell(unprocessed: List[Boolean],
                                   processed: List[Int] = Nil): List[Int] = {
      unprocessed.headOption match {
        case None => processed
        case Some(_) => countTabstopsPerCell(unprocessed.drop(1).dropWhile(!_), processed :+ (1 + unprocessed.drop(1).takeWhile(!_).length))
      }
    }

    def alignTabstops(startAndLength: Option[(Int, Int)] = None): Unit = {
      val doc = peer.getDocument.asInstanceOf[StyledDocument]
      val section = doc.getDefaultRootElement

      val allElements = (0 until section.getElementCount).map(l => section.getElement(l)).toList
      val allTextPerLine = allElements.map(el => doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset))

      val (recalcStart, recalcLength) = startAndLength match {
        case None => (0, allTextPerLine.length)
        case Some((lineNum, nofLines)) => getRecalcRange(allTextPerLine, lineNum, nofLines)
      }

      val elements = allElements.drop(recalcStart).take(recalcLength)
      val textPerLine = elements.map(el => doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset))
      val cellsPerLine = textPerLine.map("((?:[^\t\r\n]*[^ \t\r\n])?( *))\t".r.findAllMatchIn(_).toList.filter(_.matched.nonEmpty).map(m => (m.group(2).length, m.group(1))))
      def calcCellWidth(text: String): Int = {
        math.max(fm.stringWidth(text), emptyColumnWidthMinusPaddingPx) + columnPaddingPx
      }
      for ((elasticTabstopPositionsThisLine, element) <- calcElasticTabstopPositions(cellsPerLine, calcCellWidth).zip(elements)) {
        val tabStops = elasticTabstopPositionsThisLine.map(new TabStop(_))
        val attributes = new SimpleAttributeSet()
        StyleConstants.setTabSet(attributes, new TabSet(tabStops.toArray))
        val length = element.getEndOffset - element.getStartOffset
        doc.setParagraphAttributes(element.getStartOffset, length, attributes, false)
      }
    }

    protected def setElasticTabstopsDocFilter(): Unit = {

      var fontMetrics = new Canvas().getFontMetrics(elasticFont)

      object ElasticTabstopsDocFilter extends DocumentFilter {
        override def insertString(fb: FilterBypass, offset: Int, string: String, attributes: AttributeSet) {
          super.insertString(fb, offset, string, attributes)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          val lineNum = doc.getDefaultRootElement.getElementIndex(offset)
          val nofLines = string.count(_ == '\n') + 1
          alignTabstops(Some(lineNum, nofLines))
        }

        override def remove(fb: FilterBypass, offset: Int, length: Int) {
          super.remove(fb, offset, length)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          val lineNum = doc.getDefaultRootElement.getElementIndex(offset)
          val nofLines = 1
          alignTabstops(Some(lineNum, nofLines))
        }

        override def replace(fb: FilterBypass, offset: Int, length: Int, string: String, attributes: AttributeSet) {
          super.replace(fb, offset, length, string, attributes)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          val lineNum = doc.getDefaultRootElement.getElementIndex(offset)
          val nofLines = string.count(_ == '\n') + 1
          alignTabstops(Some(lineNum, nofLines))
        }
      }
      peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
    }

    val undoManager = new UndoManager
    val doc = peer.getDocument
    doc.addUndoableEditListener((event: UndoableEditEvent) => {
      val edit = event.getEdit

      // The following line no longer works in Java 9 and later
      // see: https://bugs.openjdk.java.net/browse/JDK-8190763
      //if (edit.isInstanceOf[DocumentEvent] && edit.asInstanceOf[DocumentEvent].getType != DocumentEvent.EventType.CHANGE)

      if (edit.getUndoPresentationName != "Undo style change") {
        // don't allow undoing of style changes (so we ignore tabstop changes)
        undoManager.addEdit(edit)
      }
    })

    def undoAction(): Action = {
      val action = Action("Undo") {
        try {
          if (undoManager.canUndo) {
            undoManager.undo()
            alignTabstops()
          }
        }
        catch {
          case e: CannotUndoException =>
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_Z, CTRL_MASK))
      action
    }

    def redoAction(): Action = {
      val action = Action("Redo") {
        try {
          if (undoManager.canRedo) {
            undoManager.redo()
            alignTabstops()
          }
        }
        catch {
          case e: CannotRedoException =>
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_Z, CTRL_MASK | SHIFT_MASK))
      action
    }

    listenTo(this.keys)

    reactions += {
      case kp @ KeyPressed(_, Key.Z, _, _) => {
        if (kp.peer.isControlDown()) {
          if (kp.peer.isShiftDown()) {
            redoAction.apply
          } else {
            undoAction.apply
          }
        }
      }
    }

  }

  class EditorTextPane(_elasticFont: Font, _emptyColumnWidth: Double, _columnPadding: Double,
                       var nonElasticFont: Font, var nonElasticTabSize: Int, var filesAreNonElastic: Boolean,
                       var maybePath: Option[Path])
    extends ElasticTextPane(_elasticFont, _emptyColumnWidth, _columnPadding) {

    private def getNonElasticTabSizePx: Int = {
      fm = new Canvas().getFontMetrics(nonElasticFont)
      val spaceWidthPx = fm.stringWidth(" ")
      nonElasticTabSize * spaceWidthPx
    }
    peer.setEditorKit(new StyledEditorKit() {
      override def getViewFactory: ViewFactory = new ViewFactory() {
        override def create(elem: Element): View = {
          val kind: String = elem.getName
          if (kind == AbstractDocument.ParagraphElementName) new ParagraphView(elem) {
            override def nextTabStop(x: Float, tabOffset: Int): Float = {
              val tabs = getTabSet
              if (tabs != null) return super.nextTabStop(x, tabOffset)
              getTabBase + ((x.toInt / getNonElasticTabSizePx + 1) * getNonElasticTabSizePx)
            }
          }
          else if (kind == AbstractDocument.ContentElementName) new LabelView(elem)
          else if (kind == AbstractDocument.SectionElementName) new BoxView(elem, View.Y_AXIS)
          else if (kind == StyleConstants.ComponentElementName) new ComponentView(elem)
          else if (kind == StyleConstants.IconElementName) new IconView(elem)
          else new LabelView(elem)
        }
      }
    })

    setFont(elasticFont)
    setElasticTabstopsDocFilter()

    var (currentPath, fileContents) = maybePath match {
      case None => {
        (scratchFilePath, loadScratchFile())
      }
      case Some(path) => {
        (path, loadTextFile(path).right.getOrElse(""))
      }
    }

    setNewText(if (filesAreNonElastic) spacesToSpaceTabs(fileContents, nonElasticTabSize) else fileContents)

    private var _elastic = true
    def elastic = _elastic
    def elastic_=(newElastic: Boolean) {
      if (newElastic != _elastic) {
        _elastic = newElastic
        if (_elastic) {
          // elastic on
          fm = new Canvas().getFontMetrics(elasticFont)
          setFont(elasticFont)
          val pxSettings = getPxVersionsOfEmSettings
          emptyColumnWidthMinusPaddingPx = pxSettings._1
          columnPaddingPx = pxSettings._2
          setElasticTabstopsDocFilter()
          updateText(spacesToSpaceTabs(this.text, nonElasticTabSize))
        } else {
          // elastic off
          setFont(nonElasticFont)
          peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(new DocumentFilter)
          updateText(spaceTabsToSpaces(this.text, nonElasticTabSize))
        }
      }
    }

    def changeSettings(newElasticFont: Font, newEmptyColumnWidth: Double, newColumnPadding: Double,
                       newNonElasticFont: Font, newNonElasticTabSize: Int): Unit = {
      elasticFont = newElasticFont
      emptyColumnWidth = newEmptyColumnWidth
      columnPadding = newColumnPadding
      nonElasticFont = newNonElasticFont
      nonElasticTabSize = newNonElasticTabSize

      if (_elastic) {
        fm = new Canvas().getFontMetrics(elasticFont)
        setFont(elasticFont)
        val pxSettings = getPxVersionsOfEmSettings
        emptyColumnWidthMinusPaddingPx = pxSettings._1
        columnPaddingPx = pxSettings._2
        setElasticTabstopsDocFilter()
        alignTabstops()  // force update of tabstop positions
      } else {
        setFont(nonElasticFont)
      }
    }

    private var _modified = false
    def modified = _modified
    def modified_=(newModified: Boolean) {
      if (newModified != _modified) {
        _modified = newModified
        updateWindowTitle()
      }
    }

    def updateWindowTitle(): Unit = {
      val frame = SwingUtilities.getWindowAncestor(peer).asInstanceOf[JFrame]
      if (frame != null)
        frame.setTitle(s"${if (modified) "* " else ""}${currentPath.toString} - $appName v$appVersion")
    }

    def openScratchFile(scratchFilePath: Path, settings: Settings): Unit = {
      if (currentPath != scratchFilePath && (!modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to switch to the scratch file?") == Result.Ok)) {
        currentPath = scratchFilePath
        setNewText(
          if (settings.filesAreNonElastic && _elastic) spacesToSpaceTabs(loadScratchFile, settings.nonElasticTabSize)
          else if (!settings.filesAreNonElastic && !_elastic) spaceTabsToSpaces(loadScratchFile, settings.nonElasticTabSize)
          else loadScratchFile
        )
      }
    }

    def openFile(settings: Settings): Unit = {
      if (!modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to open another file?") == Result.Ok) {
        chooseAndLoadTextFile foreach { case (loadedText, path) =>
          currentPath = path
          setNewText(
            if (settings.filesAreNonElastic && _elastic) spacesToSpaceTabs(loadedText, settings.nonElasticTabSize)
            else if (!settings.filesAreNonElastic && !_elastic) spaceTabsToSpaces(loadedText, settings.nonElasticTabSize)
            else loadedText
          )
        }
      }
    }

    def saveFile(settings: Settings): Unit = {
      val textToSave = if (settings.filesAreNonElastic) spaceTabsToSpaces(text, settings.nonElasticTabSize) else text
      saveTextFile(textToSave, currentPath)
      modified = false
    }

    def saveFileAs(settings: Settings): Unit = {
      val textToSave = if (settings.filesAreNonElastic) spaceTabsToSpaces(text, settings.nonElasticTabSize) else text
      saveTextFileAs(textToSave) foreach { path =>
        currentPath = path
        modified = false
      }
    }

    override def onChange(): Unit = {
      modified = true
    }
    override def setNewText(text: String): Unit = {
      super.setNewText(text)
      modified = false
    }
  }

}

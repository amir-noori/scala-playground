package code.playground.tui

import tui.crossterm.Event.Key
import tui.crossterm.KeyCode.{Enter, Esc}
import tui.widgets.{BlockWidget, ParagraphWidget}
import tui.widgets.tabs.TabsWidget
import tui.{Borders, Color, Constraint, Layout, Modifier, Span, Spans, Style, Terminal, Text, withTerminal}

object SearchTui {

  def main(args: Array[String]) = withTerminal { (jni, terminal) =>

    val searchApp = SearchApp(terminal, TabsState(Array("Tab0", "Tab1", "Tab2")))

    while (true) {
      terminal.draw(frame => {
        val chunks = Layout(constraints = Array(Constraint.Length(3), Constraint.Min(0))).split(frame.size)
        val titles = searchApp.appTabs.titles.map(t => Spans.from(Span.styled(t, Style.DEFAULT.fg(Color.Green))))

        val tabs = TabsWidget(
          titles = titles,
          block = Some(BlockWidget(borders = Borders.ALL, title = Some(Spans.nostyle("SEARCH TUI")))),
          highlightStyle = Style.DEFAULT.fg(Color.Yellow),
          selected = searchApp.appTabs.index
        )
        frame.renderWidget(tabs, chunks(0))
        searchApp.appTabs.index match {
          case 0 => {
            val text = Text.fromSpans(
              Spans.nostyle("This is a paragraph with several lines. You can change style your text the way you want"),
              Spans.nostyle(""),
              Spans.from(
                Span.nostyle("For example: "),
                Span.styled("under", Style.DEFAULT.fg(Color.Red)),
                Span.nostyle(" "),
                Span.styled("the", Style.DEFAULT.fg(Color.Green)),
                Span.nostyle(" "),
                Span.styled("rainbow", Style.DEFAULT.fg(Color.Blue)),
                Span.nostyle(".")
              ),
              Spans.from(
                Span.nostyle("Oh and if you didn't "),
                Span.styled("notice", Style.DEFAULT.addModifier(Modifier.ITALIC)),
                Span.nostyle(" you can "),
                Span.styled("automatically", Style.DEFAULT.addModifier(Modifier.BOLD)),
                Span.nostyle(" "),
                Span.styled("wrap", Style.DEFAULT.addModifier(Modifier.REVERSED)),
                Span.nostyle(" your "),
                Span.styled("text", Style.DEFAULT.addModifier(Modifier.UNDERLINED)),
                Span.nostyle(".")
              ),
              Spans.nostyle(
                "One more thing is that it should display unicode characters: 10€"
              )
            )
            val block = {
              val titleStyle = Style.DEFAULT
                .fg(Color.Magenta)
                .addModifier(Modifier.BOLD)
              BlockWidget(borders = Borders.ALL, title = Some(Spans.from(Span.styled("Footer", titleStyle))))
            }
            val paragraph = ParagraphWidget(text = text, block = Some(block), wrap = Some(ParagraphWidget.Wrap(trim = true)))
            frame.renderWidget(paragraph, chunks(0))
          }
          case _ =>
        }
      })

      jni.read() match {
        case key: Key => key.keyEvent.code match {
          case _: Enter => ()
          case _: Esc => System.exit(0)
          case _: tui.crossterm.KeyCode.Right => searchApp.appTabs.next()
          case _: tui.crossterm.KeyCode.Left => searchApp.appTabs.previous()
          case _ => ()
        }
        case _ => ()
      }
    }

  }

}


case class SearchApp(
                      terminal: Terminal,
                      appTabs: TabsState
                    ) {}
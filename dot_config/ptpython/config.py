from __future__ import unicode_literals

from prompt_toolkit.filters import ViInsertMode
from prompt_toolkit.key_binding.key_processor import KeyPress
from prompt_toolkit.keys import Keys
from prompt_toolkit.styles import Style
from ptpython.layout import CompletionVisualisation
from pygments.token import Token

__all__ = ('configure',)


def configure(repl):
  """
    Configuration method. This is called during the start-up of ptpython.
    :param repl: `PythonRepl` instance.
    """
  # Show function signature (bool).
  repl.show_signature = True

  # Show docstring (bool).
  repl.show_docstring = True

  # Show the "[Meta+Enter] Execute" message when pressing [Enter] only
  # inserts a newline instead of executing the code.
  repl.show_meta_enter_message = True

  # Show completions. (NONE, POP_UP, MULTI_COLUMN or TOOLBAR)
  repl.completion_visualisation = CompletionVisualisation.POP_UP

  # When CompletionVisualisation.POP_UP has been chosen, use this
  # scroll_offset in the completion menu.
  repl.completion_menu_scroll_offset = 0

  # Show line numbers (when the input contains multiple lines.)
  repl.show_line_numbers = True

  # Show status bar.
  repl.show_status_bar = True

  # When the sidebar is visible, also show the help text.
  repl.show_sidebar_help = True

  # Highlight matching parethesis.
  repl.highlight_matching_parenthesis = True

  # Line wrapping. (Instead of horizontal scrolling.)
  repl.wrap_lines = True

  # Mouse support.
  repl.enable_mouse_support = True

  # Complete while typing. (Don't require tab before the
  # completion menu is shown.)
  repl.complete_while_typing = True

  # Vi mode.
  repl.vi_mode = True

  # Paste mode. (When True, don't insert whitespace after new line.)
  repl.paste_mode = False

  # Use the classic prompt. (Display '>>>' instead of 'In [1]'.)
  repl.prompt_style = 'ipython'  # 'classic' or 'ipython'

  # Don't insert a blank line after the output.
  repl.insert_blank_line_after_output = True

  # History Search.
  # When True, going back in history will filter the history on the records
  # starting with the current input. (Like readline.)
  # Note: When enable, please disable the `complete_while_typing` option.
  #       otherwise, when there is a completion available, the arrows will
  #       browse through the available completions instead of the history.
  repl.enable_history_search = False

  # Enable auto suggestions. (Pressing right arrow will complete the input,
  # based on the history.)
  repl.enable_auto_suggest = True

  # Enable open-in-editor. Pressing C-X C-E in emacs mode or 'v' in
  # Vi navigation mode will open the input in the current editor.
  repl.enable_open_in_editor = True

  # Enable system prompt. Pressing meta-! will display the system prompt.
  # Also enables Control-Z suspend.
  repl.enable_system_bindings = True

  # Ask for confirmation on exit.
  repl.confirm_exit = False

  # Enable input validation. (Don't try to execute when the input contains
  # syntax errors.)
  repl.enable_input_validation = True

  # Use this colorscheme for the code.
  repl.use_code_colorscheme('friendly_grayscale')

  # Set color depth (keep in mind that not all terminals support true color).

  #repl.color_depth = 'DEPTH_1_BIT'  # Monochrome.
  #repl.color_depth = 'DEPTH_4_BIT'  # ANSI colors only.
  # repl.color_depth = 'DEPTH_8_BIT'  # The default, 256 colors.
  repl.color_depth = 'DEPTH_24_BIT'  # True color.

  # Syntax.
  repl.enable_syntax_highlighting = True

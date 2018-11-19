
import * as React from "react"
import * as Oni from "oni-api"

export const activate = (oni: Oni.Plugin.Api) => {
  console.log("config activated")

  // Input
  //
  // Add input bindings here:
  //
  oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))

  //
  // Or remove the default bindings here by uncommenting the below line:
  //
  // oni.input.unbind("<c-p>")

}

export const deactivate = (oni: Oni.Plugin.Api) => {
  console.log("config deactivated")
}

export const configuration = {
  "autoClosingPairs.enabled" : false,
  "commandline.mode": false,
  "editor.completions.mode": "oni",
  "editor.fontFamily": "Fira Code Retina",
  "editor.fontSize": "12px",
  "editor.textMateHighlighting.enabled" : true,
  "editor.typingPrediction"  : true, 
  "experimental.indentLines.enabled": true,
  "experimental.markdownPreview.enabled": true,
  "experimental.vcs.sidebar": true,
  "learning.enabled": false,
  "oni.hideMenu": true,
  "oni.loadInitVim": true,
  "oni.useDefaultConfig": false,
  "sidebar.default.open": false,
  "ui.animations.enabled": true,
  "ui.colorscheme": "n/a",
  "ui.fontSmoothing": "auto",
  "wildmenu.mode": true,
  "workspace.autoDetectRootFiles": [".git","node_modules",".svn","package.json",".hg",".bzr","build.xml", ".catkin_tools"],
  "workspace.autoDetectWorkspace": "always",

  // Language config
  "language.cpp.languageServer.command": "ccls",
  "language.cpp.languageServer.rootFiles": ["compile_commands.json"]

  "language.lua.languageServer.command": "lua-ls",
  "language.lua.languageServer.rootFiles": [".git"],

  "language.go.languageServer.rootFiles": [".git"], // In a git repository
  "language.go.languageServer.command": "go-langserver",
  "language.go.languageServer.arguments": ["--gocodecompletion", "--freeosmemory", "false"],

  "language.rust.languageServer.command": "rustup",
  "language.rust.languageServer.arguments": ["run", "stable", "rls"],
  "language.rust.languageServer.rootFiles": ["Cargo.toml"],
}

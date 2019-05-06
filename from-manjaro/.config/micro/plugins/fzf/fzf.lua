VERSION = "2.0.0"

MakeCommand("fzf", "fzf.fzf", 0)

function fzf()
  if CurView():CanClose() then
    RunTermEmulator("fzf", false, true, "fzf.handleFzfOutput")
  end
end

function handleFzfOutput(output)
  local strings = import("strings")
  CurView():Open(strings.TrimSpace(output))
end

AddRuntimeFile("fzf", "help", "help/fzf.md")